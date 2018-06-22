// This script looks for FIFA-sanctioned matches between the years you specify, and downloads the match information (as HTML) from the FIFA website.
require('dotenv').config()

// The years you want to find matches from (inclusive)
let fromYear = process.env.FROM_YEAR || 2014;
let toYear = process.env.TO_YEAR || 2015;

let fs = require('fs');
let mkdirp = require('mkdirp');
let waitSync = require('wait-sync');
let request = require('sync-request');

function getMatchFile(year) 
{ 
  
  let folder = `raw/matches`;
  let fileName = `${year}_matches.json`;
  let filePath = `${folder}/${fileName}`;
  if(fs.existsSync(filePath)) {
    console.log(`${fileName} exists.`);
    return 0;
  }
  
  mkdirp.sync(folder);
  let url = `http://data.fifa.com/livescores/en/internationaltournaments/matches/m/bydaterange/${year}-1-01/${year}-12-31`

  let response = request('GET', url);
  if (response.statusCode == 200) {
    let content = response.getBody().toString();
    let jsondata = content.replace("_matchesByDateRangeCallback(", "").replace("})","}");
    fs.writeFileSync(filePath, jsondata);
    console.log(`Fetched ${fileName}`);
  }
  else {
    console.error(fileName + ": error " + response.statusCode);
  }
  
  return 1;
};

// main
for (let year = fromYear; year <= toYear; year++) {
      if (getMatchFile(year))
        waitSync(3);
}

