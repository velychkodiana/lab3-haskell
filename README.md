# Text Processing in Haskell — Lab 3 

This project implements a text processing program in Haskell for analyzing text from a programming textbook.  
The program reads a text file, normalizes spacing, splits the text into sentences, counts the number of words in each sentence, and outputs the result in a formatted table sorted by sentence length.

---

##  Source of the Text

The sample input text is taken from the educational Haskell book:

**"Learn You a Haskell for Great Good!"** by Miran Lipovača  
Website: https://learnyouahaskell.com/  
License: Creative Commons BY-NC-SA

---

##  Program Features

- Reads text from `text.txt`
- Replaces multiple spaces and tabs with a single space
- Splits text into sentences based on `.`, `!`, `?`
- Uses custom data types:
  - `Symbol`
  - `Word'`
  - `Punctuation`
  - `Sentence`
- Counts the number of words in each sentence
- Sorts sentences in ascending order by word count
- Outputs formatted table:
  - Indented paragraph formatting
  - Blank line between sentences
  - Controlled word wrapping (no word-breaking)
- Includes unit tests using **Hspec**

---

##  Project Structure

```

lab3-haskell
├─ app
│   └─ Main.hs                # Program entry point (I/O + formatted output)
│
├─ src
│   └─ TextProcessing.hs      # Core logic and custom data types
│
├─ test
│   └─ Main.hs                # Test suite written with Hspec
│
├─ text.txt                   # Input text from the Haskell textbook
│
├─ package.yaml               # Project configuration
└─ stack.yaml                 # Stack configuration

````

---

##  Requirements

Before running the project, install:

- **GHC** — Glasgow Haskell Compiler  
- **Stack** — build tool for Haskell

### macOS installation (Homebrew):

```bash
brew install ghc
brew install stack
````

---

##  Build & Run

To compile and run the program:

```bash
stack build
stack run
```

The program will automatically read:

```
text.txt
```

---

##  Running Tests

To run the unit tests:

```bash
stack test
```

Expected output example:

```
normalizeSpaces
  replaces multiple spaces with a single one
splitIntoSentences
  correctly splits text into sentences
wordCount
  counts words correctly

Finished in 0.002s
3 examples, 0 failures
```

---

**Author:** *Diana Velychko*  
**Year:** 2025

Laboratory Work №3 — Functional Programming (Haskell)
