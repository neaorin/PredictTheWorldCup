let matches = require('./processed/matches/matches.json');

let fs = require('fs');
let json2csv = require('json2csv');
 
try {
  let matches_csv = json2csv({ data: matches });
  fs.writeFileSync('./processed/matches/matches.csv', matches_csv);
} catch (err) {
  console.error(err);
}