// This script takes FIFA-sanctioned competitions, optionally filters them, and combines them into a single file.

// FIFA-sanctioned competitions which you are NOT interested in. These will not be downloaded
let dumpCompetitions = ['Futsal', 'Beach', 'U-17', 'U-20', 'Youth'];

"use strict";

let fs = require('fs');
let mkdirp = require('mkdirp');
let dateFormat = require('dateformat');

let path = 'raw/competitions/';
let files = fs.readdirSync(path)

let competitions = new Array();

for (let i=0; i<files.length; i++) {
	let buf = JSON.parse(fs.readFileSync(path + files[i]));
	let year = files[i].split('_')[0];

	for (let month = 1; month <= 12; month++) {
		let dateStr = dateFormat(new Date(year, month, 1),
			 "isoDateTime");
		
		dateStr = dateStr.substr(0, dateStr.indexOf('T'));

		if (buf[dateStr] != null) {
			for (let j=0; j<buf[dateStr].length; j++) {
				
				let comp = buf[dateStr][j];
				let dumps = dumpCompetitions.filter(function(item) {
					return (comp.CupName.indexOf(item) > -1);
				});
				if (dumps.length > 0) {
					continue;
				}
				
				let compids = competitions.filter(function(item) {
					return (item.IdCupSeason == comp.IdCupSeason);
				});
				if (compids.length == 0) {
					competitions.push(comp);
				}
			}
		}
	}
}

mkdirp.sync('./processed/competitions/')
fs.writeFileSync('./processed/competitions/competitions.json', JSON.stringify(competitions, null, 3));
