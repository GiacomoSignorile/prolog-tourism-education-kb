# Prolog Tourism Knowledge Base

This project implements a knowledge base for tourism information using Prolog,
based on a GBS schema. It includes entity hierarchies, facts about points of
interest, services, transportation, and rules for querying this information.

## Prerequisites

*   SWI-Prolog (Version 8.x or later recommended)

## How to Run

1.  Open SWI-Prolog.
2.  Navigate to the project directory using `cd('path/to/prolog-tourism-kb').`
3.  Load the main file:
    ```prolog
    ?- ['tourism.pl'].
    ```

4.  You can now run queries, for example:
    ```prolog
    ?- is_cultural_site(louvre).
    ?- poi_in_city(Poi, 'Paris City Center').
    ```

## Files

*   `schemas`: Defines the entity hierarchy and schema structure.
*   `tourism`: Contains the facts, rules, and example queries.
*   `.gitignore`: Specifies intentionally untracked files that Git should ignore.
*   `README.md`: This file.