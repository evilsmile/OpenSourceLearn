// Import the page's CSS. Webpack will know what to do with it.
import '../styles/app.css'

// Import libraries we need.
import { default as Web3 } from 'web3'
import { default as contract } from 'truffle-contract'

import coinArtifact from '../../build/contracts/MyAdvancedToken.json'

const Coin = contract(coinArtifact);

const TokenMgr =  {

    start : function() {
        Coin.setProvider(web3.currentProvider);

        let coinIns;
        Coin.deployed().then(function(ins) {
            coinIns = ins;
            Coin.Transfer().watch(function(err, resp) {
                if (!err) {
                    console.log("Transfer Resp:", resp.args);
                } else {
                    alert("Transfer ERRRRR: " + err);
                }
            });

            Coin.Approve().watch(function(err, resp) {
                if (!err) {
                    console.log("Approve Resp:", resp.args);
                } else {
                    alert("Approve ERRRRR: " + err);
                }
            });
        });
    },

    queryTokenBal: function() {
        const self = this;

        let coinIns;

        Coin.deployed().then(function(ins) {
            coinIns = ins;
            var addr = document.getElementById("address").value;
            return coinIns.balanceOf.call(addr);
        }).then(function(val) {
            if (val) {
                document.getElementById("balance").innerHTML = val.valueOf();
            }
        });
    },

};

window.TokenMgr = TokenMgr;

window.addEventListener('load', function () {
  // Checking if Web3 has been injected by the browser (Mist/MetaMask)
  if (typeof web3 !== 'undefined') {
    console.warn(
      'Using web3 detected from external source.' +
      ' If you find that your accounts don\'t appear or you have 0 Crowdsale,' +
      ' ensure you\'ve configured that source properly.' +
      ' If using MetaMask, see the following link.' +
      ' Feel free to delete this warning. :)' +
      ' http://truffleframework.com/tutorials/truffle-and-metamask'
    )
    // Use Mist/MetaMask's provider
    window.web3 = new Web3(web3.currentProvider)
  } else {
    console.warn(
      'No web3 detected. Falling back to http://127.0.0.1:9545.' +
      ' You should remove this fallback when you deploy live, as it\'s inherently insecure.' +
      ' Consider switching to Metamask for development.' +
      ' More info here: http://truffleframework.com/tutorials/truffle-and-metamask'
    )
    // fallback - use your fallback strategy (local node / hosted node + in-dapp id mgmt / fail)
    window.web3 = new Web3(new Web3.providers.HttpProvider("http://39.108.222.178:9879"));
  }

  TokenMgr.start()
})
