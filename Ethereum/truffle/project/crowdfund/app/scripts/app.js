// Import the page's CSS. Webpack will know what to do with it.
import '../styles/app.css'
import '../styles/buttons.css'

// Import libraries we need.
import { default as Web3 } from 'web3'
import { default as contract } from 'truffle-contract'

// Import our contract artifacts and turn them into usable abstractions.
import crowdSaleArtifact from '../../build/contracts/Crowdsale.json'


// Crowdsale is our usable abstraction, which we'll use through the code below.
const Crowdsale = contract(crowdSaleArtifact)

// The following code is simple to show off interacting with your contracts.
// As your needs grow you will likely need to change its form and structure.
// For application bootstrapping, check out window.addEventListener below.
let accounts;
let account;
let gasAccount;
let beneficiaryAddr;
let deadline = 0;
let countDownTimer;

const ETHER = 1e18;

const App = {
  start: function () {
    const self = this;

    // Bootstrap the Crowdsale abstraction for Use.
    Crowdsale.setProvider(web3.currentProvider)

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
      account = accounts[0];
      gasAccount= accounts[8];

      self.refreshAcountBalanceList();

    });

    let crowdSale;
    Crowdsale.deployed().then(function (instance) {

        crowdSale = instance;
        return crowdSale.deadline.call();

    }).then(function(value) {

        deadline = value.valueOf();
        console.log("dead line: ", deadline);
        return crowdSale.getTokenSymbol();

    }).then(function(symbol) {

        console.log("symbol: ", symbol);
        document.getElementById("tokens_amount_tip").innerHTML = "Hold Tokens(" + symbol.valueOf() + "): ";   

    }).then(function() {
        crowdSale.FundTransfer().watch(function(error, resp) {
            if (!error) {
                var args = resp.args;
                console.log("fund transfer info: ", args);
                if (args.isContribution) {
                    alert("Fund transfer succss!");
                    self.refreshAmountRaised();
                } else {
                    alert("Withdraw success!");
                }
            } else {
                alert("FundTransfer ERRRRROR!" + error);
                return;
            }
        });

        crowdSale.GoalReached().watch(function(error, resp) {
            if (!error) {
                var args = resp.args;
                console.log("goalReached: ", args);
            } else {
                alert("GoalReached Event ERRRRROR!" + error);
                return;
            }
        });
    });


    self.refreshBeneficiaryAddr();
    self.refreshCrowsaleAmount();
    self.refreshAmountRaised();
    self.refreshPrice();
    self.refreshGoalReached();
    self.refreshCrowdsaleClosedStatus();
  },

  showOrHideCrowdsale: function(hideFlag) {
      if (hideFlag) {
          document.getElementById("crowdsale_input_div").style.display = "none";
      } else {
          document.getElementById("crowdsale_input_div").style.display = "block";
      }
  },

  refreshAcountBalanceList: function() {
      const self = this;

      for (var i in accounts) {

          var acnt = accounts[i];

          var tr = document.createElement('tr');
          tr.setAttribute("id", "acnt_bal_"+acnt);

          var tdAcnt = document.createElement('td');
          if (acnt == beneficiaryAddr) {
             tdAcnt.style.cssText += "font-weight:bold";
          }
          tdAcnt.innerHTML = acnt;
          tr.appendChild(tdAcnt);

          var tdPay = document.createElement('td');
          var btn = document.createElement('button');
          btn.textContent = "pay";
          btn.setAttribute("id", "crowdsale_btn_"+acnt);
          btn.setAttribute("class", "button button-3d button-action button-circle button-jumbo");
          btn.addEventListener('click', function() {

              self.showOrHideCrowdsale(false);

              var thisAcnt = this.getAttribute("id").slice(14);
              document.getElementById("sender").innerHTML = thisAcnt;

              web3.eth.getBalance(thisAcnt, function(err, bal) {
                  if (err != null) {
                      alert("Get bal failed");
                      return;
                  }
                  var balContent = parseInt(bal.valueOf())/ETHER;
                  document.getElementById("balance_show").innerHTML = balContent;
                  console.log("balContent: ", balContent);
              });

              Crowdsale.deployed().then(function (instance) {
                  return instance.balanceOf.call(thisAcnt)
                  .then(function(val) {
                      if (val) {
                          var crowdedAmount = parseInt(val.valueOf())/ETHER;
                          document.getElementById("crowded_amount").innerHTML = crowdedAmount;
                      }
                  }).then(function() {
                    return instance.tokenBalanceOf.call(thisAcnt);
                  }).then(function(amt) {
                      console.log("amooount: ", amt);
                      document.getElementById("tokens_amount").innerHTML = amt;
                  });
              });

          });
          tdPay.appendChild(btn);
          tr.appendChild(tdPay);

          document.getElementById("acnt_bal_tbl").append(tr);
      }
  },

  setStatus: function (message) {
    const status = document.getElementById('status')
    status.innerHTML = message
  },

  payCrowdsale: function () {
    const self = this;
    var amount = parseFloat(document.getElementById("pay_amount").value) * ETHER;
    var sender = document.getElementById("sender").innerHTML;

    web3.eth.getBalance(sender, function(err, res) {
        console.log("sender old bal:", res.toNumber());
    });

    console.log("Get pay sale request[ amount: ", amount, ", sender: ", sender, "] to [", Crowdsale.address, "]");

    Crowdsale.deployed().then(function(instance) {
        return web3.eth.sendTransaction({
            from: sender,
            to: Crowdsale.address,
            value: amount,
            gas: "158575"
        }, function (err, txHash) {

            if (!err) {
                console.log("send ok. TXHash: ", txHash)
            } else {
                alert("Pay ERRR: " + err);
            }

            web3.eth.getBalance(sender, function(err, res) {
                console.log("sender new bal:", res.toNumber());
            });
        });
    });

  },

  safeWithdrawal: function () {
    const self = this

    let crowdSale
    Crowdsale.deployed().then(function (instance) {
      crowdSale = instance
      return crowdSale.safeWithdrawal({from: beneficiaryAddr});
    }).then(function (value) {
      console.log("safeWithdrawal success!");
    }).catch(function (e) {
      console.log(e);
    })
  },

  checkGlobalReached: function () {
    const self = this

    let crowdSale
    Crowdsale.deployed().then(function (instance) {
        crowdSale = instance;
        return crowdSale.checkGlobalReached({from: gasAccount});
    }).then(function () {
        self.refreshCrowdsaleClosedStatus();
        self.refreshAmountRaised();
        self.refreshGoalReached();
        console.log("check succ");
    }).catch(function (e) {
        alert("ERROR: " + e);
    });
  },

  refreshBeneficiaryAddr: function() {
      const self = this;

      let crowdSale;
      Crowdsale.deployed().then(function (instance) {
          crowdSale = instance;
          return crowdSale.beneficiary.call();
      }).then(function(value) {
          var benefAddr = document.getElementById("benef_addr_label");
          beneficiaryAddr = value.valueOf();
          benefAddr.innerHTML = value.valueOf();
      }).catch(function(e) {
          console.log(e);
      });
  },


  refreshCrowsaleAmount: function() {
      const self = this;

      let crowdSale;
      Crowdsale.deployed().then(function (instance) {
          crowdSale = instance;
          return crowdSale.fundingGoal.call();
      }).then(function(value) {
          var crowdsaleAmount = document.getElementById("crowdsale_amount_label");
          crowdsaleAmount.innerHTML = parseInt(value.valueOf())/ETHER + " ETHER";
      }).catch(function(e) {
          console.log(e);
      });
  },

  refreshGoalReached: function() {
      const self = this;

      let crowdSale;
      Crowdsale.deployed().then(function (instance) {
          crowdSale = instance;
          return crowdSale.fundingGoalReached.call();
      }).then(function(value) {
          var goalReached = document.getElementById("goal_reached_label");
          goalReached.innerHTML = value.valueOf();
      }).catch(function(e) {
          console.log(e);
      });
  },

  refreshCrowdsaleClosedStatus: function() {
      const self = this;

      let crowdSale;
      Crowdsale.deployed().then(function (instance) {
          crowdSale = instance;
          return crowdSale.crowdsaleClosed.call();
      }).then(function(value) {
          var closedLabel = document.getElementById("closed_label");
          closedLabel.innerHTML = value.valueOf();
      }).catch(function(e) {
          console.log(e);
      });
  },


  refreshPrice: function() {
      const self = this;

      let crowdSale;
      Crowdsale.deployed().then(function (instance) {
          crowdSale = instance;
          return crowdSale.price.call();
      }).then(function(value) {
          var priceLabel = document.getElementById("price_label");
          priceLabel.innerHTML = parseInt(value.valueOf())/ETHER + " ETHER";
      }).catch(function(e) {
          console.log(e);
      });
  },

  refreshAmountRaised: function() {
      const self = this;

      let crowdSale;
      Crowdsale.deployed().then(function (instance) {
          crowdSale = instance;
          return crowdSale.amountRaised.call();
      }).then(function(value) {
          var amountRaisedLabel = document.getElementById("raised_amount_label");
          amountRaisedLabel.innerHTML = parseInt(value.valueOf())/ETHER + " ETHER";
      }).catch(function(e) {
          console.log(e);
      });
  },

  countDown: function() {

      const self = this;

      var now = new Date();
      var nowTimestamp = Math.floor(now.getTime()/1000);
      var interval = deadline - nowTimestamp;
      if (deadline > 0 && interval >= 0) {
          var days = Math.floor(interval/60/60/24);
          var hours = Math.floor(interval/60/60%24);
          var minutes = Math.floor(interval/60%60);
          var seconds = Math.floor(interval%60);
          document.getElementById("countdown_show").innerHTML = days + " Days " + hours + " Hours " + minutes + " Mins " + seconds + " Secs";
      } else {
          // Wait 10 seconds
          if (interval <= -10) {
            self.checkGlobalReached();
            clearInterval(countDownTimer);
          }
      }
  },

}

window.App = App

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

  countDownTimer = setInterval("App.countDown()", 1000);

  App.start()
})
