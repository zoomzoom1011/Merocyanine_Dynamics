myOBJDIR = obj
mySRCDIR = sub_dynamics
mySRCFILES = abs_osc.F95 evec_out.F95 normalization_pl.F95 \
		abs_out.F95 get_tct.F95 \
		abs_spec.F95 hamiltonian.F95 para_out.F95 \
		allocate_hev.F95 hamiltonian_out.F95 pl_osc.F95 \
		build_molecule_geometry.F95 index_mon_state.F95 pl_out.F95 \
		calc_coupling.F95 index_sys_state.F95 pl_spec.F95 \
		character_function.F95 kount_out.F95 read_para_file.F95 \
		common_variables.F95 merocyanine.F95 sign_fxn.F95 \
		delta_ES.F95 delta_NZ.F95 volap.F95 pl.F95\
		dsyev_diagonalize.F95 normalization_ab.F95 \
        get_population.F95 get_current_state.F95 \
        get_alpha_coeff.F95 get_coherence.F95 data_collection.F95
        # finish_dislin.F95 setup_dislin.F95
myOBJFILES = $(mySRCFILES:.F95=.o)
myDEP = common_variables.mod
myEXE = merocyanine.exe
vpath %.o ${myOBJDIR}
vpath %.mod ${myOBJDIR}
vpath %.F95 ${mySRCDIR}
FLINKER = gfortran
# LIBS = -L/path/to/libs -llapack -lblas -ldislin -lXt -lm
LIBS = -L/path/to/libs -llapack -lblas

${myEXE} : ${myOBJFILES} ${myDEP:.mod=.o}
	-${FLINKER} $(addprefix ${myOBJDIR}/,$(^F)) -o $@ -I${myOBJDIR} $(LIBS)

%.o : %.F95 ${myDEP:.mod=.o}
	-${FLINKER} -c ${mySRCDIR}/$(<F) -o ${myOBJDIR}/$(@F) -I${myOBJDIR} $(LIBS)

${myDEP:.mod=.o} : ${myDEP:.mod=.F95}
	-${FLINKER} -c $< -o ${myOBJDIR}/${@:.mod=.o} -J${myOBJDIR} $(LIBS)

clean : 
	@echo cleaning up
	@-rm ${myOBJDIR}/*.o ${myOBJDIR}/*.mod 2>/dev/null || true
	@-rm ${myEXE}
	@clear
