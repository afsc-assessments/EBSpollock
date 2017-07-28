ifdef ComSpec
    RM=del /F /Q
    COPY=copy
else
    RM=rm -rf
    COPY=cp
endif

all: pm_p

pm_p: pm_p.tpl 
	@admb  pm_p.tpl 

clean:
	@$(RM) pm_p.cpp
	@$(RM) pm_p.htp
	@$(RM) pm_p.obj
