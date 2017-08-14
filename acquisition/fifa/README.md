# Reading information about FIFA-sanctioned matches

## Requirements

- [Node.js](https://nodejs.org/en/)

## Howto

1. Install [Node.js](https://nodejs.org/en/)

2. Open a shell into this folder.

3. Install the packages specified in `package.json`

`npm install`

4. (Optional) Change the time period for which you'd like to fetch matches. The default period is **2014 - 2015**.

Create a file called `.env` in this folder and fill in the values you require:

```
FROM_YEAR=2009
TO_YEAR=2016
```
Save the `.env` file.

5. Get a list of competitions.

`node fifa_get_competitions.js`

Output file(s): `./raw/competitions/*.json`

6. Process the list of competitions into a single file.

`node fifa_process_competitions.js`

Output file(s): `./processed/competitions/competitions.json`

7. Get a list of matches.

`node fifa_get_matches.js`

Output file(s): `./raw/matches/**/*.html`

8. Process each raw HTML matches into a correspondent `.json` file:

`node fifa_process_matches.js process`

Output file(s): `./processed/matches/**/*.json`

9. Concatenate the matches files into a single `matches.json` file:

`node fifa_process_matches.js concat`

Output file(s): `./processed/matches/matches.json`

10. (Optional) Transform the `matches.json` file into a `.csv` file.

`node fifa_transformcsv.js`

Output file(s): `./processed/matches/matches.csv`