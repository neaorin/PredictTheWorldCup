// This script looks for FIFA-sanctioned matches between the years you specify, and downloads the match information (as HTML) from the FIFA website.
require('dotenv').config()

// FIFA-sanctioned competitions which you are NOT interested in. These will not be downloaded
let dumpCompetitions = ['Futsal', 'Beach', 'U-17', 'U-20', 'Youth'];

// The years you want to find matches from
let fromYear = process.env.FROM_YEAR || 2014;
let toYear = process.env.TO_YEAR || 2015;

let http = require('http');
let fs = require('fs');
let mkdirp = require('mkdirp');
let dateFormat = require('dateformat');
let waitSync = require('wait-sync');
let request = require('sync-request');

function getMatchFile (IdCupSeason, date) 
{ 
  
  let folder = "raw/matches/" + IdCupSeason + "/";  
  let fileName = IdCupSeason +  "_" + date + "_matches.html";
  let filePath = folder + fileName;
  if(fs.existsSync(filePath)) {
    console.log(fileName + " exists.");
    return 0;
  }
  
  mkdirp.sync(folder);
  let url = "http://www.fifa.com/live/world-match-centre/library/fixtures/bymonth/idcupseason=" + IdCupSeason +  "/date=" + date + "/_matches.html";

  let response = request('GET', url);
  if (response.statusCode == 200) {
    fs.writeFileSync(filePath, response.getBody());
    console.log("Fetched " + fileName);
  }
  else {
    console.error(fileName + ": error " + response.statusCode);
  }
  
  return 1;
};

// main

for (let year = fromYear; year <= toYear; year++) {
  let competitions = require('./raw/competitions/' + year + '_competitionMonthList.json');
  	for (let month = 1; month <= 12; month++) {
      
      let dateStr = dateFormat(new Date(year, month, 1),
        "isoDateTime");      
      dateStr = dateStr.substr(0, dateStr.indexOf('T'));
      
      if (competitions[dateStr] != null) {
        for (let j=0; j<competitions[dateStr].length; j++) {
          
          let competition = competitions[dateStr][j];

          let dumps = dumpCompetitions.filter(function(item) {
            return (competition.CupName.indexOf(item) > -1);
          });
          if (dumps.length > 0) {
            console.log("(" + competition.IdCupSeason + ") " + competition.CupName + " dumped.");
            continue;
          }
          
          if (getMatchFile(competition.IdCupSeason, dateStr.replace('-','').replace('-','')) == 1)
            waitSync(3);
        }
      }
    }
  
}

