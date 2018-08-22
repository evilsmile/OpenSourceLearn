var glob = require('glob');

const path = require('path')
const CopyWebpackPlugin = require('copy-webpack-plugin')

var entries = function() {
    var srcDir = path.resolve(process.cwd(), 'app');
    var jsDir = path.resolve(srcDir, 'scripts');
    var entryFiles = glob.sync(jsDir + "/*.{js,jsx}");
    var map = {};

    for (var i = 0; i < entryFiles.length; i++) {
        var filePath = entryFiles[i];
        var filename = filePath.substring(filePath.lastIndexOf("\/")+1, filePath.lastIndexOf("."));
        map[filename] = filePath;
    }
    return map;
};

var htmls = function() {
    var srcDir = path.resolve(process.cwd(), 'app');
    var htmlFiles = glob.sync(srcDir + "/*.html");
    var arrays = [];

    for (var i = 0; i < htmlFiles.length; i++) {
        var filePath = htmlFiles[i];
        var filename = filePath.substring(filePath.lastIndexOf("\/")+1);
        arrays.push({from: filePath, to: filename});
    }
    return arrays;
};

module.exports = {
  entry: entries() ,
  mode: 'production',
  output: {
    path: path.resolve(__dirname, 'build'),
    filename: '[name].js',
  },
  plugins: [
    // Copy our app's index.html to the build folder.
    new CopyWebpackPlugin(htmls())
  ],
  devtool: 'source-map',
  module: {
    rules: [
      { test: /\.s?css$/, use: [ 'style-loader', 'css-loader', 'sass-loader' ] },
      {
        test: /\.js$/,
        exclude: /(node_modules|bower_components)/,
        loader: 'babel-loader',
        query: {
          presets: ['env'],
          plugins: ['transform-react-jsx', 'transform-object-rest-spread', 'transform-runtime']
        }
      },
      { 
        test: /\.(png|jpg)$/,
        loader: 'url-loader?limit=8192&name=imgs/.*'
      }
    ]
  }
}

