
SOLNAME=newcoin
#SOLNAME=tokenERC20
#SOLNAME=simple_token

all:${SOLNAME}
.PHONY: ${SOLNAME}

${SOLNAME}:${SOLNAME}.DONE

${SOLNAME}.DONE:${SOLNAME}.sol
	solcjs --bin --abi $< && touch $@

echo:
	@for f in $$(ls -1 *${SOLNAME}* | egrep -vw "sol|DONE"); \
		do echo -e "\n【$$f】"; \
		cat $$f ; \
		echo; \
		done


# estimateGas
es:
	@echo -e "eth.estimateGas({data:\"0x$$(cat *${SOLNAME}*bin)\"});"

# Update this value by the command upper
GASFEE=1281600

INSTALL_TMPFILE=install.js.temp
INSTALL_FILE=install_${SOLNAME}.js

install:
	@cp ${INSTALL_TMPFILE} ${INSTALL_FILE}
	@sed -i  \
	    -e 's/--{DATA}--/'$$(cat *${SOLNAME}*bin)'/g'      \
		-e 's/--{GASFEE}--/'${GASFEE}'/g'                  \
		-e 's/--{ABI}--/'$$(cat *${SOLNAME}*abi)'/g'          \
		${INSTALL_FILE}
	@cat ${INSTALL_FILE}

help:
	@echo "eth.getCode(contractInstance.address) : [check contract deployed!]"
