BayesB <-function(y,Z,startpi,startdf,startscale,alphapi,betapi,truepi, truedef, truescale,numiter,skip,burnIn,Seed=1002,Save.at="results")  
#  startpi is defined by the proportion of markers that ARE associate with genes
{   # START OF FUNCTION

	  set.seed(Seed)  

	  library(msm)

	  if(numiter <=burnIn) stop ("Total Number of MCMC iterations should exceed number of burnin iterations")

	  #filename=paste("LDsimu_",Seed,"_",2,sep="")
	  #data(filename)

	  #Save.at=paste("results/",sep=" ")

	  # Parameters
	  nSNP=dim(Z)[2]  #number of SNP
	  nanim=dim(Z)[1]; #number of animals

	  # Vectors for saved samples of hyperparameters
	  if (!truedef)    {defsave   =  array(0,numiter/skip) }
	  if (!truescale)  {scalesave =  array(0,numiter/skip) }
	  if (!truepi)     {pisave    =  array(0,numiter/skip) } 
	  varesave = array(0,numiter/skip)

	  # To monitor MH acceptance ratios on degrees of freedom parameter
	  alphadef_save = array(0,(numiter-burnIn))

	  # input data
	  nrecords = length(y)
	  X = array(1,nrecords)
	  dimX = 1
	  W = as.matrix(cbind(X,Z1.marker))
	  rm(Z)
	  Wstacked=as.vector(W);

	  # specify prior degrees of belief on genetic variance
	  # specify  prior scale on genetic variance
	  # inital values   

	  def=startdf			 # Starting Bayes A df 
	  scalea  = startscale    	 # Starting scale parameter
	  pi = startpi                   # Starting pi parameter   (probability of being a gene!)
	  cdef       = 0.5		 # cdef is scale parameter for proposal density on df
	  alphacheck = 0
	  alphaQTLvar= 0			 # to monitor BayesB QTLvar rejection rates 
	  alphatot1= 0
	  WWdiag     = apply(W,2,crossprod) # diagonals are important

	  # prior for residual variance
	  nu_e     = -1          # i.e. corresponding to Gelman's prior.
	  scale_e  = 0

	  # used to monitor Metropolis Hastings acceptance ratio for df
	  dimW     = dim(W)[2]  # number of columns = # of location parameters

	  # initialize a few arrays
	  theta      = array(0,nSNP+1)
	  SNPeff     = array(0,nSNP)
	  varq       = array(scalea,nSNP)  # initialize each SNP variance to overall scale parameter
	  SNPvar     = array(0,nSNP)       # used to take running sum of SNP specific variances
	  SNPvarlast = array(0,nSNP)
	  postprob   = array(0,nSNP)       # indicator variable for inclusion of SNP in model
	                                   # later used to determine posterior probability...unique to BayesB
	  alphametrovar = array(0,nSNP)    # used to monitor MH acceptance rates for included genes.  
  
	  # This is for computing mean and variance of each SNP effect.
	  Mlast =    array(0,dimW)
	  Qlast =    array(0,dimW)
	  Mcurrent = array(0,dimW)
	  Qcurrent = array(0,dimW)
	  storeDbarj<-c()

	  # adjust y
	  ycorr = y - as.vector(W%*%theta)   # adjust obs by everything before starting iterations
	  # so this is the residual.

	  # use this to set up the starting value for the residual variance
	  vare = crossprod(ycorr)/(nrecords-dimX)
  
	  starttime<-proc.time()
	  timeSNP = 0
	  timetotal=0

	  # mcmc sampling
	  for (iter in 1:numiter) 
	  {  #"START" OF MCMC SAMPLING

	     itersave = iter-burnIn
	     if (itersave ==1) 
	     {
	        alphametrovar = array(0,nSNP)  # reinitialize to 0 post burn-in
	        postprob = array(0,nSNP) # reinitialize to 0 post burn-in
	     }

	     #####################################################################################
	     # sample intercept
	     #####################################################################################
	     ycorr    = ycorr + W[,1]*theta[1]
	     rhs      = sum(ycorr)/vare        # 1'(y-W*theta)/sigmae
	     invLhs   = 1.0/(nrecords/vare)    # 1/(sigmae/n)		
	     meancond = rhs*invLhs                          
	     theta[1] = rnorm(1,meancond,sqrt(invLhs))       
	     ycorr    = ycorr - W[,1]*theta[1]  # take it off again to create new "residual"	

	     #####################################################################################	
	     # sample variance & effect for each SNP (BayesA)	
	     ####################################################################################
	     if (pi == 1)  
	     { # "START" OF BAYESA
	       #####################################################################################
	       # sample variance for each SNP (Bayes A)	
	       #####################################################################################
	       varq = (scalea*def + SNPeff*SNPeff)/rchisq(nSNP,def+1)  # joint sample

	       #####################################################################################
	       # sample effect for each SNP (Bayes A)	
	       #####################################################################################
	       startSNP = proc.time()
	       BayesC<- .Call("BayesAC",nSNP,dimX,nanim,Wstacked,WWdiag, theta, ycorr,varq,vare,SNPeff)
	       theta=BayesC[[1]]
	       ycorr=BayesC[[2]]
	       varq=BayesC[[3]]
	       vare=BayesC[[4]]
	       SNPeff=BayesC[[5]]
	     }   # "END" FOR BAYESA

	      #####################################################################################
	      # sample effect for each SNP (Bayes B)	
	      #####################################################################################
	     if (pi < 1 )  
	     {   # "START" OF BAYESB

	         startSNP = proc.time()
	         G1=0
	         #####################################################################################
	         # sample effect and variance for each SNP (Bayes B)	
	         #####################################################################################
	         BayesB<-.Call("BayesBC",nSNP,dimX,nanim,Wstacked,WWdiag,theta,ycorr,varq,vare,SNPeff,postprob,scalea,def,alphametrovar,iter,pi,G1) 
			# "END" FOR EACH SNP IN BAYESB
			theta=BayesB[[1]]
			ycorr=BayesB[[2]]
			varq=BayesB[[3]]
			vare=BayesB[[4]]
			SNPeff=BayesB[[5]]
			postprob=BayesB[[6]]
			alphametrovar=BayesB[[7]]
			G1=BayesB[[8]][1] #add G1
			def=BayesB[[9]]
			scalea=BayesB[[10]]
			pi=BayesB[[11]]
			alpha1=BayesB[[12]][1]
			alphacheck=BayesB[[13]][1]		 		 	
	     } # "END" OF BAYESB

	     if (itersave>1) 
	     {     # "START" OF COMPUTE POSTERIOR MEANS & VARIANCES
	           Mcurrent = Mlast + (theta-Mlast)/itersave
	           Qcurrent = Qlast + (itersave-1)*((theta-Mlast)^2)/itersave
	           Mlast = Mcurrent
	           Qlast = Qcurrent 

	           SNPvar=SNPvarlast + (varq-SNPvarlast)/itersave
	           SNPvarlast=SNPvar
	     }  # "END" OF COMPUTE POSTERIOR MEANS & VARIANCES
	     if (itersave==1) {Mlast = theta;SNPvarlast=varq;} 

	     #####################################################################################	
	     #   sample residual variance
	     #####################################################################################
	     vare = ( crossprod(ycorr) +scale_e )/rchisq(1,nrecords+nu_e)   # prior on vare used
	     #  scale parameter should be e`e + Se,
	     #  degrees of freedom should be n + ve

	    #####################################################################################
	     # sample degrees of freedom
	     #####################################################################################
	     if (!truedef)  
	     {  #"START" OF SAMPLE THE DEGREES OF FREEDOM
	        for (cycle in 1:10){
	        def0=def
	        logdef0 = log(def0)
	        logdef1 = rtnorm(1,logdef0,cdef,upper=log(100))   # proposal log df 
	        def1 = exp(logdef1)
	        lfullcd0 = G1*(0.5*def0*(log(scalea)+logdef0-log(2))-lgamma(0.5*def0))
	        lfullcd1 = G1*(0.5*def1*(log(scalea)+logdef1-log(2))-lgamma(0.5*def1))
	        lfullcd0 = lfullcd0 - (0.5*def0+1)*sum(log(varq[which(varq>0)]))-(0.5*def0*scalea)*sum(1/varq[which(varq>0)]);
	        lfullcd1 = lfullcd1 - (0.5*def1+1)*sum(log(varq[which(varq>0)]))-(0.5*def1*scalea)*sum(1/varq[which(varq>0)]);
	        lfullcd0 = lfullcd0- 2*log(1+def0)+logdef0   # prior + jacobian                                      
	        lfullcd1 = lfullcd1- 2*log(1+def1)+logdef1   # prior + jacobian                          
	        alpha1 = exp(lfullcd1-lfullcd0)
	        alpha1 = min(alpha1,1)
	        if (alpha1 < 1)
	        { 
		  u = runif(1)
		  if (u<alpha1) {def = def1}
		  else def = def0
	        }
	       else {def = def1;}
	       alphatot1=alphatot1+alpha1
	       if ((cycle%%skip == 0) & (iter <= burnIn))
	       {
	         alphacheck = alphatot1/skip;
	         if (alphacheck > .70) { cdef = cdef*1.2}
	         else if(alphacheck  < .20) {cdef = cdef*.7}     # tune Metropolis sampler #
	         alphatot1 = 0;
	       }
	       else if ((cycle%%skip == 0) & (iter > burnIn)) 
	       {
	         alphacheck = alphatot1/skip;
	         alphadef_save[itersave/skip] = alphacheck
	         alphatot1 = 0
	       }
	       }
	     }   # "END" OF SAMPLE THE DEGREES OF FREEDOM  

      
	     #####################################################################################
	     #  sample scale#
	     #####################################################################################
	     if (!truescale) 
	      {  #"START" OF SAMPLE THE SCALE PARAMETER 
	            shape_g = 0.5*(G1*def+1)
	            scale_g = 0.5*def*sum(1/varq[which(varq>0)])
	            scalea=rgamma(1,shape=(shape_g+0.1),rate=(scale_g+0.1))   
	      } #"END" OF SAMPLE THE SCALE PARAMETER 

	      #####################################################################################
	      #      sample pi
	      #####################################################################################
	       if (!truepi) 
	       { 
	           pi = rbeta(1,alphapi+G1,betapi+nSNP-G1) 
	       } #"END" OF SAMPLE THE PI PARAMETER 

	       ###########################################################################################
	       #store MCMC samples at every "skip"
	       ###########################################################################################
	       if (iter%%skip == 0) 
	        {
	            varesave[iter/skip] = vare
	            if(!truedef) {defsave[iter/skip] = def}
	            if(!truescale) {scalesave[iter/skip] = scalea}
	            if(!truepi) {pisave[iter/skip] = pi}
	        }

	       endSNP = proc.time()
	       timeSNP = timeSNP + (endSNP[1]-startSNP[1]) 

	       endtime = proc.time()
	       timetotal = timetotal + (endtime[1]-starttime[1])

	       if (iter%%1000==0) 
	       {

	       #############################################################################################
	       #    PROCESSING MCMC SAMPLES
	       #############################################################################################
	       hyperparameters = as.matrix(varesave)
	       colnames(hyperparameters) = "varesave"
	       if (!truescale) {hyperparameters = cbind(hyperparameters,scalesave) }
	       if (!truedef) {hyperparameters = cbind(hyperparameters,defsave) }
	       if (!truepi) {hyperparameters = cbind(hyperparameters,pisave) }
	       rownames(hyperparameters) = seq(skip,numiter,skip)
	       save(iter,hyperparameters,file=paste(Save.at, "Hyperparameters",Seed,".RData",sep="")) 
 
	       SNPtimepercycle = timeSNP/iter
	       timepercycle=timetotal/iter
	       save(iter,timepercycle,SNPtimepercycle,file=paste(Save.at, "time",Seed,".RData",sep=""))

	       }# "END" OF PROCESSING MCMC SAMPLING
               
	       if (iter%%1000==0 & itersave>1 ) 
	       {

	       #############################################################################################
	       #    PROCESSING MCMC SAMPLES
	       #############################################################################################
	       meanmu=Mcurrent[dimX]
	       meang= Mcurrent[(dimX+1):dimW]
	       varg = Qcurrent[(dimX+1):dimW]/itersave
	       save(iter,meanmu,meang,varg,SNPvar,file = paste(Save.at,"EffectsResults",Seed,".RData",sep=""))

	       #  monitor MH acceptance rates over cycle for the degrees of freedom term.
	       save(iter,alphadef_save,file=paste(Save.at, "alphadef",Seed,".RData",sep=""))

	       } # "END" OF PROCESSING MCMC SAMPLING

	  } # "END" OF MCMC SAMPLING

}
