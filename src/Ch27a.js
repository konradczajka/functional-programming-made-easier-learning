const chalk = require('chalk');

exports._chalk = (styles, str) => {
    var result = chalk;
    styles.forEach(style => result = result[style]);
    return result(str);
}

// exports._chalk = (styles, str) => {
//     for (var i = 0, c = chalk; i < styles.length; ++i)
//         c = c[styles[i]];
//     return c(str);
// }