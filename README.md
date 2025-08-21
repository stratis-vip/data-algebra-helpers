# Data.Algebra.Helpers

Βοηθητική βιβλιοθήκη για Μαθηματικά Λυκείου σε Haskell"

## Σκοπός

Η υποστήριξη του εγχειρήματος με ενιαίες λειτουργίες που αφορούν στην διατύπωση
κσι εμφάνιση των μαθηματικών εννοιών, όσο και οι οι κοινές βοηθητικές πράξεις.

## Τρόπος χρήσης

1. Δημιουργία ```cabal.project``` ()που απαιτείται για να εισαχθεί η βιβλιοθήκη
από το [github](https://github.com/stratis-vip/data-algebra-helpers) τουλάχιστον
μέχρι να ανεβεί στο [hackage](https://hackage-content.haskell.org/)

```cabal
packages: .

source-repository-package
  type: git
  location: https://github.com/stratis-vip/data-algebra-helpers.git
  tag: v0.1.0.14
```

2. Προσθήκη στο cabal αρχείο, στα build-depends του ```algebra-helpers```

```cabal
  -- Other library packages from which modules are imported.
    build-depends:    base ^>=4.20.0.0
                      , hspec 
                      , process
                      , text
                      , algebra-helpers -- <- this is it
```

3. Εισαγωγή σε κάθε αρχείο με ```import Data.Algebra.Helpers```

## Συναρτήσεις

### Κειμένου

1. __formatSampleSpace__

    Δημιουργεί την μαθηματική απεικόνιση με {} ενός, δειγματικού χώρου, για κάθε 'tuple list'. Αν ο δειγματικός χώρος είναι κενός, τότε εμφανίζει τον χαρακτήρα ∅.

  ```haskell
 formatSampleSpace :: (Show a) => String -> [a] -> String
```

2. __clearScreen__

    Καθαρίζει το τερματικό!

```haskell
clearScreen :: IO ()
```

3. __prepareText__

    Εκτυπώνει το κείμενο s σε προκαθορισμένη διάσταση οθόνη 80 χαρακτήρες.
    Οι 80 χαρακτήρες είναι η προεπιλογή. Αν θέλουμε μπορούμε να το αλλάξουμε.

  ```haskell
prepareText :: String -> IO ()
prepareText s = putStrLn $ concat . splitSentence s $ 80
```

### Υπολογισμού

1. __combinations__

    Υπολογίζει τον δειγματικό χώρο με επανατοποθέτηση των δειγματικών σημείων

    ```haskell
    combinations :: [a] -> [(a, a)]
    ```

2. __premutables__

    Υπολογίζει τον δειγματικό χώρο χωρίς επανατοποθέτηση των δειγματικών σημείων

    ```haskell
   premutables :: (Eq a) => [a] -> [(a, a)]
    ```

### Διάφορες
