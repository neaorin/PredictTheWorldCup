// This script looks for FIFA-sanctioned competitions between the years you specify, and downloads the description files from the FIFA website.

require('dotenv').config()

// The years you want to find matches from
let fromYear = process.env.FROM_YEAR || 2014;
let toYear = process.env.TO_YEAR || 2015;

// ------------------------

let http = require('http');
let fs = require('fs');
let mkdirp = require('mkdirp');

function getCompetitionsFile(year) 
{ 
  let folder = "./raw/competitions/";  
  let fileName = year + "_competitionMonthList.json";
  let filePath = folder + fileName;

  fs.exists(filePath, function (exists) {
    if (exists) {
      console.log(fileName + " exists.");
      return;
    }

    let file = fs.createWriteStream(filePath);
    let url = "http://www.fifa.com/live/common/world-match-centre/year=" + year + "/gender=m/_competitionMonthList.js";
    http.get(url, function(response) {
      response.pipe(file);
      console.log("Fetched " + fileName);
    });
  });
};


// main
mkdirp.sync('./raw/competitions')

for (let year = fromYear; year <= toYear; year++) {
  getCompetitionsFile(year); 
}

