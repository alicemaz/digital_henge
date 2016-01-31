"use strict";

const config = require("./config.json");
//fields: year, month, day (ints) / type ("lunar" or "solar", only for eclipses)
const seasons = require("./seasons.json");
const eclipses = require("./eclipses.json");
const zodiac = require("./zodiac.json");

const Twit = require("twit");
const T = new Twit(config);

//current date/time
const d = new Date();

//array indexed 0-7 starting with new, corresponds to moon_phase function
const phases_emoji = [
	"\uD83C\uDF11",
	"\uD83C\uDF12",
	"\uD83C\uDF13",
	"\uD83C\uDF14",
	"\uD83C\uDF15",
	"\uD83C\uDF16",
	"\uD83C\uDF17",
	"\uD83C\uDF18"
];

//what it says on the tin
const sun_emoji = "\u2600\uFE0F";
const earth_emoji = "\uD83C\uDF0E";

//this function isn't mine
function moon_phase(year, month, day) {
	let r = year % 100;
	r %= 19;
	if (r>9){ r -= 19;}
	r = ((r * 11) % 30) + parseInt(month) + parseInt(day);
	if (month<3){r += 2;}
	r -= ((year<2000) ? 4 : 8.3);
	r = Math.floor(r+0.5)%30;
	r = (r < 0) ? r+30 : r;
	//my additions, convert 0-29 to 0-8 then change 8 to 0
	r = Math.floor(r/3.625)
	r = r & 7;

	return r;
}

//compare today's moon phase to yesterday's, if it differs, then tweet it
function check_moon() {
	let yd = new Date(d);
	yd.setDate(yd.getDate()-1);

	if(moon_phase(d.getFullYear(), d.getMonth(), d.getDate()) !== moon_phase(yd.getFullYear(), yd.getMonth(), yd.getDate()))
		return phases_emoji[moon_phase(d.getFullYear(), d.getMonth(), d.getDate())];
}

//checks to see if there's a solstice or equinox today, switchcase for where to put the earth based on month if yes
function check_season() {
	for(var prop in seasons) {
		if(seasons[prop].year === d.getFullYear()
        && seasons[prop].month === d.getMonth()
        && seasons[prop].day === d.getDate()) {
			switch(seasons[prop].month) {
				case 2:
					return earth_emoji + "\n" + sun_emoji;
				case 5:
					return earth_emoji + sun_emoji;
				case 8:
					return sun_emoji + "\n" + earth_emoji;
				case 11:
					return sun_emoji + earth_emoji;
			}
		}
	}
}

//checks to see if there's an eclipse today, tweet it
function check_eclipse() {
	for(var prop in eclipses) {
		if(eclipses[prop].year === d.getFullYear()
        && eclipses[prop].month === d.getMonth()
        && eclipses[prop].day === d.getDate()) {
			switch(eclipses[prop].month) {
				case 1:
				case 2:
				case 3:
					return eclipses[prop].type === "solar"
						? earth_emoji + "\n" + phases_emoji[4] + "\n" + sun_emoji
						: phases_emoji[4] + "\n" + earth_emoji + "\n" + sun_emoji;
				case 4:
				case 5:
				case 6:
					return eclipses[prop].type === "solar"
						? earth_emoji + phases_emoji[2] + sun_emoji
						: phases_emoji[2] + earth_emoji + sun_emoji;
				case 7:
				case 8:
				case 9:
					return eclipses[prop].type === "solar"
						? sun_emoji + "\n" + phases_emoji[0] + "\n" + earth_emoji
						: sun_emoji + "\n" + earth_emoji + "\n" + phases_emoji[0];
				case 10:
				case 11:
				case 0:
					return eclipses[prop].type === "solar"
						? sun_emoji + phases_emoji[6] + earth_emoji
						: sun_emoji + earth_emoji + phases_emoji[6];
			}
		}
	}
}

//check to see if the zodiac sign has changed today
function check_zodiac() {
	for(var prop in zodiac) {
		if(zodiac[prop].month === d.getMonth() && zodiac[prop].day === d.getDate())
			return zodiac[prop].sign;
	}
}

function tweet(status) {
    //console.log(status);
    T.post("statuses/update", { status: status}, function(err, data, response) { });
}

//just call this every time the script runs, most times it won't do anything
function try_everything() {
    //testing zzz
	//d.setDate(d.getDate()-2);

    let statuses = [];

    statuses.push(check_moon());
    statuses.push(check_zodiac());
    statuses.push(check_season());
    statuses.push(check_eclipse());

    let i = 0;
    for(let status of statuses) {
        if(status) {
            setTimeout(() => tweet(status), i*1000*60*5);
            i++;
        }
    }
}

try_everything();
