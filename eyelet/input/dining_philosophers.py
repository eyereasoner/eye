"""
Dining Philosophers — deterministic output
Python 3.8+
"""
import threading
import time

N_PHILOSOPHERS = 5
MEALS_PER_PHIL = 3
THINK_TIME     = 0.05   # seconds
EAT_TIME       = 0.05

# Forks (still real locks, even though contention is now controlled)
forks = [threading.Lock() for _ in range(N_PHILOSOPHERS)]

# --------  deterministic turn-taking  --------
turn = 0                       # whose turn is it to eat?
turn_cond = threading.Condition()
# --------------------------------------------

def philosopher(i: int) -> None:
    left  = forks[i]
    right = forks[(i + 1) % N_PHILOSOPHERS]

    global turn
    for meal in range(1, MEALS_PER_PHIL + 1):
        # deterministic thinking
        print(f"💭 Phil {i} thinking (meal {meal}/{MEALS_PER_PHIL})")
        time.sleep(THINK_TIME)

        # wait for our turn
        with turn_cond:
            while turn != i:
                turn_cond.wait()

            # now it's our slot; grab forks in the usual order
            with left, right:
                print(f"  🍴 Phil {i} eating (meal {meal})")
                time.sleep(EAT_TIME)

            # done; pass the turn to next philosopher
            turn = (turn + 1) % N_PHILOSOPHERS
            turn_cond.notify_all()

    print(f"✅ Phil {i} finished all meals")

def main() -> None:
    threads = [
        threading.Thread(target=philosopher, args=(i,), name=f"P{i}")
        for i in range(N_PHILOSOPHERS)
    ]
    for t in threads:
        t.start()
    for t in threads:
        t.join()

    print("\n🎉 Everyone ate, no deadlock, deterministic order!")

if __name__ == "__main__":
    main()

