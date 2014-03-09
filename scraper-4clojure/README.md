# scraper-4clojure

Scrapes saved solutions from 4Clojure.com, one solution per file. Uses Selenium.

## Usage

Use from repl. There are two important functions:
  scrape-4clojure [user pwd dir max]
    Scrapes every problem from range (1, (inc max)) into the targeted dir.
  scrape-single [user pwd dir problem]
    Scrapes problem number problem into the targeted dir.

## License

Copyright © 2014 Travis Moy

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
