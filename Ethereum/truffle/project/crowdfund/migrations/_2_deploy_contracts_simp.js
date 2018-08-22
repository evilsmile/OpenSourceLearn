var SimpleToken = artifacts.require('./SimpleToken.sol')
var Crowdsale = artifacts.require('./Crowdsale.sol')

module.exports = function (deployer, network, accounts) {
    return deployer.deploy(SimpleToken, {from: accounts[0], value:1e18})
        .then(() => {
            const ifSuccessfulSendTo = web3.eth.accounts[5];
            const fundingGoalInEthers = 10;
            // 30 MIN
            var durationInSeconds = 30 * 60;
            //var durationInSeconds = 3 * 60;

            if (network == "test") {
                durationInSeconds = 1;
            }

            const etherCostOfEachToken = 1e15 ;
            const addressOfTokenUsedAsReward = SimpleToken.address;

            // Deploy and charge.
            return deployer.deploy(Crowdsale, ifSuccessfulSendTo, fundingGoalInEthers, 
                durationInSeconds, etherCostOfEachToken, addressOfTokenUsedAsReward, 
                {from:accounts[0], value:1e17});
        });
}
