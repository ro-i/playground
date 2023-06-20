//
// Code based on https://parthsl.wordpress.com/2019/01/20/pthread-locks-mutex-vs-spilocks-vs-futex/
//
// Also some ideas from http://www.rkoucha.fr/tech_corner/the_futex.html
//
// Compile with: gcc -std=c17 -pedantic -Wall -Wextra -pthread ./locks.c -o locks
//
#define _GNU_SOURCE

#include <errno.h>
#include <limits.h>
#include <linux/futex.h>
#include <pthread.h>
#include <stdatomic.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/syscall.h>
#include <sys/types.h>
#include <time.h>
#include <unistd.h>

#ifndef LOOPS
#define LOOPS 10000000
#endif

#define DIE(msg)            \
    do {                    \
        perror(msg);        \
        exit(EXIT_FAILURE); \
    } while (0)

typedef struct {
    atomic_uint value;
    atomic_uint waiters;
} futex_t;

typedef atomic_flag spinlock_t;

typedef struct Lock Lock;
struct Lock {
    char* description;
    volatile void* lock;
    void (*lock_destroy)(Lock* lock);
    void (*lock_func)(Lock* lock);
    void (*lock_init)(Lock* lock);
    void (*unlock_func)(Lock* lock);
};

static long long a[LOOPS];
static long long iter;

static futex_t _futex;
static pthread_mutex_t _mutex;
static pthread_spinlock_t _spinlock;
static spinlock_t _my_spinlock;

static inline void futex_init(Lock* lock)
{
    futex_t* futex = (futex_t*)lock->lock;
    futex->value = 0;
    futex->waiters = 0;
}

static inline void futex_destroy(Lock* lock)
{
    (void)lock;
}

static inline void futex_lock(Lock* lock)
{
    unsigned zero = 0;
    futex_t* futex = (futex_t*)lock->lock;

    int cnt = 0;
    while (atomic_compare_exchange_strong(&futex->value, &zero, 1)) {
        __asm__ volatile("pause");
        if (cnt > 500) {
            atomic_fetch_add(&futex->waiters, 1);
            syscall(SYS_futex, &futex->value, FUTEX_WAIT_PRIVATE, NULL, NULL, 0);
            atomic_fetch_sub(&futex->waiters, 1);
        } else {
            cnt++;
        }
    }
}

// Wake up 1 (arbitrary) thread or use INT_MAX to wake up all threads.
static inline void futex_unlock(Lock* lock)
{
    futex_t* futex = (futex_t*)lock->lock;

    //atomic_fetch_sub(&futex->value, 1);
    atomic_store_explicit(&futex->value, 0, memory_order_release);
    if (atomic_load(&futex->waiters)) {
        syscall(SYS_futex, &futex->value, FUTEX_WAKE_PRIVATE, INT_MAX, NULL, NULL, 0);
    }
}

static inline void mutex_init(Lock* lock)
{
    if (pthread_mutex_init((pthread_mutex_t*)lock->lock, NULL)) {
        DIE("pthread_mutex_init");
    }
}

static inline void mutex_destroy(Lock* lock)
{
    if (pthread_mutex_destroy((pthread_mutex_t*)lock->lock)) {
        DIE("pthread_mutex_destroy");
    }
}

static inline void mutex_lock(Lock* lock)
{
    if (pthread_mutex_lock((pthread_mutex_t*)lock->lock)) {
        DIE("pthread_mutex_lock");
    }
}

static inline void mutex_unlock(Lock* lock)
{
    if (pthread_mutex_unlock((pthread_mutex_t*)lock->lock)) {
        DIE("pthread_mutex_unlock");
    }
}

static inline void my_spinlock_init(Lock* lock)
{
    atomic_flag_clear((spinlock_t*)lock->lock);
}

static inline void my_spinlock_destroy(Lock* lock)
{
    (void)lock;
}

static inline void my_spinlock_lock(Lock* lock)
{
    while (atomic_flag_test_and_set((spinlock_t*)lock->lock)) {
        sleep(0);
    }
}

static inline void my_spinlock_unlock(Lock* lock)
{
    atomic_flag_clear((spinlock_t*)lock->lock);
}

static inline void spinlock_init(Lock* lock)
{
    if (pthread_spin_init((pthread_spinlock_t*)lock->lock, PTHREAD_PROCESS_PRIVATE)) {
        DIE("pthread_spin_init");
    }
}

static inline void spinlock_destroy(Lock* lock)
{
    if (pthread_spin_destroy((pthread_spinlock_t*)lock->lock)) {
        DIE("pthread_spin_destroy");
    }
}

static inline void spinlock_lock(Lock* lock)
{
    if (pthread_spin_lock((pthread_spinlock_t*)lock->lock)) {
        DIE("pthread_spin_lock");
    }
}

static inline void spinlock_unlock(Lock* lock)
{
    if (pthread_spin_lock((pthread_spinlock_t*)lock->lock)) {
        DIE("pthread_spin_lock");
    }
}

void* consumer(void* ptr)
{
    Lock* lock = (Lock*)ptr;

    for (bool running = true; running;) {
        lock->lock_func(lock);

        if (iter < LOOPS) {
            a[iter] = a[iter] * 2;
            iter++;
        } else {
            running = false;
        }

        lock->unlock_func(lock);
    }

    return NULL;
}

void measure(Lock* lock)
{
    pthread_t t1, t2;
    struct timespec tv1, tv2;

    if (clock_gettime(CLOCK_MONOTONIC, &tv1)) {
        DIE("clock_gettime");
    }

    iter = 0;

    if (pthread_create(&t1, NULL, consumer, (void*)lock)) {
        DIE("pthread_create");
    }
    if (pthread_create(&t2, NULL, consumer, (void*)lock)) {
        DIE("pthread_create");
    }

    if (pthread_join(t1, NULL)) {
        DIE("pthread_join");
    }
    if (pthread_join(t2, NULL)) {
        DIE("pthread_join");
    }

    if (clock_gettime(CLOCK_MONOTONIC, &tv2)) {
        DIE("clock_gettime");
    }

    printf("Result for %s - %ld ms\n",
        lock->description,
        (tv2.tv_sec - tv1.tv_sec) * 1000 + (tv2.tv_nsec - tv1.tv_nsec) / 1000000);
}

int main()
{
    Lock futex = {
        .description = "futex",
        .lock = &_futex,
        .lock_destroy = futex_destroy,
        .lock_func = futex_lock,
        .lock_init = futex_init,
        .unlock_func = futex_unlock
    };
    Lock mutex = {
        .description = "mutex",
        .lock = &_mutex,
        .lock_destroy = mutex_destroy,
        .lock_func = mutex_lock,
        .lock_init = mutex_init,
        .unlock_func = mutex_unlock
    };
    Lock my_spinlock = {
        .description = "my_spinlock",
        .lock = &_my_spinlock,
        .lock_destroy = my_spinlock_destroy,
        .lock_func = my_spinlock_lock,
        .lock_init = my_spinlock_init,
        .unlock_func = my_spinlock_unlock
    };
    // Lock spinlock = {
    //     .description = "spinlock",
    //     .lock = &_spinlock,
    //     .lock_destroy = spinlock_destroy,
    //     .lock_func = spinlock_lock,
    //     .lock_init = spinlock_init,
    //     .unlock_func = spinlock_unlock
    // };
#define LOCK_NUM 3
    Lock locks[LOCK_NUM] = { futex, mutex, my_spinlock }; //, spinlock };

    for (int i = 0; i < LOCK_NUM; i++) {
        Lock* lock = &locks[i];

        srand(time(NULL));
        for (unsigned long long i = 0; i < LOOPS; i++) {
            a[i] = rand();
        }

        lock->lock_init(lock);

        measure(lock);

        lock->lock_destroy(lock);
    }

    return EXIT_SUCCESS;
}
