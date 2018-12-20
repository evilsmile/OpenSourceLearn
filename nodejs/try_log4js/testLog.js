var log4js = require("log4js");
var log4js_config = require("./logConf.json");
log4js.configure(log4js_config);

console.log("log_start start!")

var LogFile = log4js.getLogger('log_date');
//var LogFile = log4js.getLogger('log_file');
//var LogFile = log4js.getLogger('console');

LogFile.trace("This is a Log4js-Test");
LogFile.debug("We write logs with log4js");
LogFile.info("We write logs with log4js");
LogFile.warn("We write logs with log4js");
LogFile.error("We write logs with log4js");
