var fs = require("fs");
var api_stuff = JSON.parse(fs.readFileSync(__dirname + "/config.json", "utf8"));

var Twit = require("twit");
var T = new Twit(
{
	consumer_key: api_stuff.key,
	consumer_secret: api_stuff.secret,
	access_token: api_stuff.token,
	access_token_secret: api_stuff.token_secret
})

//current date/time
var d = new Date();
//d.setDate(d.getDate()-1);
//console.log(d);

//array indexed 0-7 starting with new, corresponds to moon_phase function
var phases_emoji =
[
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
var sun_emoji = "\u2600\uFE0F";
var earth_emoji = "\uD83C\uDF0E";

//these are arrays of objects, eg seasons[3].day
//fields: year, month, day (ints) / type ("lunar" or "solar", only for eclipses)
var seasons = JSON.parse(fs.readFileSync(__dirname + "/seasons.json", "utf8"));
var eclipses = JSON.parse(fs.readFileSync(__dirname + "/eclipses.json", "utf8"));
var zodiac = JSON.parse(fs.readFileSync(__dirname + "/zodiac.json", "utf8"));

//and just a simple global for the tweet, my functions possibly put stuff in it, main method tweets if !== ""
var final_tweet = "";

//this function isn't mine
function moon_phase(year, month, day)
{
	var r = year % 100;
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
function check_moon()
{
	var yd = new Date(d);
	yd.setDate(yd.getDate()-1);

	if(moon_phase(d.getFullYear(), d.getMonth(), d.getDate()) !== moon_phase(yd.getFullYear(), yd.getMonth(), yd.getDate()))
		final_tweet = phases_emoji[moon_phase(d.getFullYear(), d.getMonth(), d.getDate())];
}

//checks to see if there's a solstice or equinox today, switchcase for where to put the earth based on month if yes
function check_season()
{
	for(var prop in seasons)
	{
		if(seasons[prop].year === d.getFullYear() && seasons[prop].month === d.getMonth() && seasons[prop].day === d.getDate())
		{
			switch(seasons[prop].month)
			{
				case 2:
					final_tweet = earth_emoji + "\n" + sun_emoji;
					break;
				case 5:
					final_tweet = earth_emoji + sun_emoji;
					break;
				case 8:
					final_tweet = sun_emoji + "\n" + earth_emoji;
					break;
				case 11:
					final_tweet = sun_emoji + earth_emoji;
					break;
			}
		}
	}
}

//checks to see if there's an eclipse today, tweet it
function check_eclipse()
{
	for(var prop in eclipses)
	{
		if(eclipses[prop].year === d.getFullYear() && eclipses[prop].month === d.getMonth() && eclipses[prop].day === d.getDate())
		{
			switch(eclipses[prop].month)
			{
				case 1:
				case 2:
				case 3:
					eclipses[prop].type === "solar"
						? final_tweet = earth_emoji + "\n" + phases_emoji[4] + "\n" + sun_emoji
						: final_tweet = phases_emoji[4] + "\n" + earth_emoji + "\n" + sun_emoji;
					break;
				case 4:
				case 5:
				case 6:
					eclipses[prop].type === "solar"
						? final_tweet = earth_emoji + phases_emoji[2] + sun_emoji
						: final_tweet = phases_emoji[2] + earth_emoji + sun_emoji;
					break;
				case 7:
				case 8:
				case 9:
					eclipses[prop].type === "solar"
						? final_tweet = sun_emoji + "\n" + phases_emoji[0] + "\n" + earth_emoji
						: final_tweet = sun_emoji + "\n" + earth_emoji + "\n" + phases_emoji[0];
					break;
				case 10:
				case 11:
				case 0:
					eclipses[prop].type === "solar"
						? final_tweet = sun_emoji + phases_emoji[6] + earth_emoji
						: final_tweet = sun_emoji + earth_emoji + phases_emoji[6];
					break;
			}
		}
	}
}

//check to see if the zodiac sign has changed today
function check_zodiac()
{
	for(var prop in zodiac)
	{
		if(zodiac[prop].month === d.getMonth() && zodiac[prop].day === d.getDate())
			final_tweet = zodiac[prop].sign;
	}

}

//unlike my joke bots where it's whatever to run them local, digital_henge depends on the system clock and I never ever want to trigger a tweet manually
//comment out try_everything(); and use this instead
//actually whatever I'll write it later if I need it but basically while loop to increment the time, console.log rather than tweet
function test_local()
{
}

//just call this every time the script runs, most times it won't do anything
//the reason for this terrible method is Heroku's scheduler doesn't let you schedule jobs, so I have to run it hourly
//the new reason now that I'm running everything off my own machine is that I don't feel like refactoring it
function try_everything()
{
	final_tweet = "";

	switch(d.getHours())
	{		
		case 10:
			check_zodiac();
			break;
		case 12:
			check_season();
			break;
		case 15:
			check_eclipse();
			break;
		case 20:
			check_moon();
			break;
	}

	if(final_tweet !== "")
		T.post("statuses/update", { status: final_tweet}, function(err, data, response) { });
}

try_everything();
