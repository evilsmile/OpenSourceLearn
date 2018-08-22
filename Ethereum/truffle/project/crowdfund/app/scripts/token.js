// Import the page's CSS. Webpack will know what to do with it.
import '../styles/token_manager.css'
import '../styles/buttons.css'

// Import libraries we need.
import { default as Web3 } from 'web3'
import { default as contract } from 'truffle-contract'

import coinArtifact from '../../build/contracts/MyAdvancedToken.json'

const Coin = contract(coinArtifact);

let accounts;

const TokenMgr =  {

    start : function() {
        Coin.setProvider(web3.currentProvider);

        let coinIns;
        Coin.deployed().then(function(ins) {
            coinIns = ins;
            coinIns.Transfer().watch(function(err, resp) {
                if (!err) {
                    console.log("Transfer Resp:", resp.args);
                } else {
                    alert("Transfer ERRRRR: " + err);
                }
            });

            coinIns.Approval().watch(function(err, resp) {
                if (!err) {
                    console.log("Approval Resp:", resp.args);
                } else {
                    alert("Approval ERRRRR: " + err);
                }
            });
        });

        // Get the initial account balance so it can be displayed.
        web3.eth.getAccounts(function (err, accs) {
            if (err != null) {
                alert('There was an error fetching your accounts.')
            return
            }

            if (accs.length === 0) {
                alert("Couldn't get any accounts! Make sure your Ethereum client is configured correctly.")
            return
            }

            accounts = accs;
        });
    },

    queryTokenBal: function() {
        const self = this;

        let coinIns;

        Coin.deployed().then(function(ins) {
            coinIns = ins;
            var addr = document.getElementById("query_addr").value;
            return coinIns.balanceOf.call(addr);
        }).then(function(val) {
            if (val) {
                document.getElementById("balance").innerHTML = val.valueOf();
            }
        });
    },

    transfer: function() {
        const self = this;

        let coinIns;
        Coin.deployed().then(function(ins) {
            coinIns = ins;
            
            var fromAddr = document.getElementById("transfer_from_addr").value;
            var toAddr = document.getElementById("transfer_to_addr").value;
            var amount = parseInt(document.getElementById("transfer_amt").value);

            if (!fromAddr || !toAddr || !amount) {
                alert("ERROR: empty input!");
                return;
            }

            return coinIns.transfer(fromAddr, toAddr, amount, {from: accounts[0]});
        }).then(function() {
            console.log("transfered.");
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
