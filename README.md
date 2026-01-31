# Wordle in x86 Assembly

## Introduction

Wordle is a word-guessing game where the program randomly selects a 5-letter word, and the player tries to guess it.

- **Game Start:**  
  The player begins at the splash screen and starts the game by pressing the `Enter` key.

- **Gameplay:**  
  The player has 6 chances to input a valid 5-letter word to guess the correct answer.

  - If the guess is **incorrect**, the program provides feedback:
    - Correct letters in the correct position.
    - Correct letters in the wrong position.

  - If the guess is **correct**, the player receives a score based on the number of attempts used and can choose to continue playing with the accumulated score.

- **Game Over:**  
  If the player fails to guess the word within 6 attempts, the program reveals the correct word and settles the score.

This project is implemented entirely in **x86 Assembly (32-bit)** using the library `Irvine32.inc`.



Below is a screenshot of the game:

<img width="1568" height="1004" alt="image" src="https://github.com/user-attachments/assets/de700444-ddbe-42f8-83c9-8cde4ab677cb" />
