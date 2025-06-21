#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_TAPE 100     // Maximum size of the tape
#define HASH -1          // Represents a blank cell on the tape
#define HALT -2          // Special state value indicating the machine should halt

// Possible directions the tape head can move
typedef enum { LEFT, RIGHT, STAY } Move;

// Structure to define a transition in the Turing machine
typedef struct {
    int state;       // Current state
    int read;        // Current symbol under the head
    int write;       // Symbol to write
    Move move;       // Direction to move
    int next_state;  // Next state after the move
} Transition;

// Tape structure to hold the symbols, head position, and length
typedef struct {
    int tape[MAX_TAPE]; // The tape itself
    int head;           // Current position of the head
    int length;         // Effective length of the tape
} Tape;

// Define the state transition rules for the add-1 Turing machine
Transition transitions[] = {
    {0, 0, 0, RIGHT, 0},
    {0, 1, 1, RIGHT, 0},
    {0, HASH, HASH, LEFT, 1},
    {1, 0, 1, STAY, HALT},
    {1, 1, 0, LEFT, 1},
    {1, HASH, 1, STAY, HALT},
};

// Find the index of a matching transition for the current state and symbol
int find_transition(int state, int read_symbol) {
    for (size_t i = 0; i < sizeof(transitions)/sizeof(Transition); i++) {
        if (transitions[i].state == state && transitions[i].read == read_symbol) {
            return (int)i;  // Cast back to int for compatibility
        }
    }
    return -1; // No transition found
}

// Reverse a segment of an array (used conceptually for reverse logic, but unused here)
void reverse(int* arr, int len) {
    for (int i = 0; i < len/2; i++) {
        int tmp = arr[i];
        arr[i] = arr[len - i - 1];
        arr[len - i - 1] = tmp;
    }
}

// Main function to simulate the Turing machine
void compute(int input[], int input_len) {
    Tape tape;
    memset(tape.tape, HASH, sizeof(tape.tape)); // Initialize all cells to blank

    // Copy input to tape
    for (int i = 0; i < input_len; i++) {
        tape.tape[i] = input[i];
    }

    tape.head = 0;
    tape.length = input_len > 0 ? input_len : 1;

    int state = 0; // Initial state

    // Turing machine execution loop
    while (state != HALT) {
        int symbol = tape.tape[tape.head];
        int t_index = find_transition(state, symbol);
        if (t_index == -1) {
            printf("No transition found. Halting.\n");
            break;
        }

        Transition t = transitions[t_index];
        tape.tape[tape.head] = t.write; // Write the new symbol
        state = t.next_state;           // Update to next state

        // Move the head
        if (t.move == RIGHT) {
            tape.head++;
            if (tape.head >= tape.length) {
                tape.tape[tape.head] = HASH; // Extend tape with blank
                tape.length++;
            }
        } else if (t.move == LEFT) {
            if (tape.head == 0) {
                // Simulate infinite tape to the left by shifting right
                memmove(&tape.tape[1], &tape.tape[0], tape.length * sizeof(int));
                tape.tape[0] = HASH;
                tape.length++;
            } else {
                tape.head--;
            }
        }
        // If STAY, do nothing
    }

    // Print the result
    printf("Result: ");
    for (int i = 0; i < tape.length; i++) {
        if (tape.tape[i] == HASH) continue; // Skip blanks in final output
        printf("%d", tape.tape[i]);
    }
    printf("\n");
}

// Main function with test cases
int main() {
    // Input tapes to be simulated
    int tapes[][6] = {
        {1, 0, 1, 0, 0, 1},
        {1, 0, 1, 1, 1, 1},
        {1, 1, 1, 1, 1, 1},
        {}, // Empty tape
    };

    int lengths[] = {6, 6, 6, 0}; // Corresponding lengths of input tapes

    // Run the Turing machine on each test input
    for (int i = 0; i < 4; i++) {
        printf("compute([");
        for (int j = 0; j < lengths[i]; j++) {
            printf("%d", tapes[i][j]);
            if (j < lengths[i] - 1) printf(",");
        }
        printf("]): ");
        compute(tapes[i], lengths[i]);
    }

    return 0;
}

