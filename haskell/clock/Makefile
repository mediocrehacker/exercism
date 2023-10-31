test: 
	stack test	

.PHONY=watch
watch: 
	@find ./ \( -name "*.hs" \) | entr $(MAKE)
