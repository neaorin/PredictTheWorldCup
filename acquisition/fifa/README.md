# Acquiring data about FIFA-sanctioned international matches

## Description

This suite of scripts crawls the [FIFA website](http://www.fifa.com/) and reads data about FIFA-sanctioned competitions and matches. The data is saved locally.

## Requirements

- [Node.js](https://nodejs.org/en/)

## Howto

1. Install [Node.js](https://nodejs.org/en/)

2. Clone this repository, then open a command shell in this folder, which is `(repository root folder)/acquisition/fifa`.

3. Install the packages specified in `package.json`

    `npm install`

4. (Optional) Change the time period for which you'd like to fetch matches. The default period is **2014 - 2015**.

    Create a file called `.env` in this folder and fill in the values you require:

    ```text
    FROM_YEAR=1950
    TO_YEAR=2017
    ```
    Save the `.env` file.

5. Get match data from FIFA website.

    `node fifa_get_matches.js`

    Output file(s): `./raw/matches/*.json`

6. Concatenate the match files into a single `matches.json` file:

    `node fifa_process_matches.js`

    Output file(s): `./processed/matches/matches.json`

7. (Optional) Transform the `matches.json` file into a `.csv` file.

    `node fifa_transformcsv.js`

    Output file(s): `./processed/matches/matches.csv`