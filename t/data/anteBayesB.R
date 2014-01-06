anteBayesB <-function(y,Z,startpi,startdf,startscale,alphapi,betapi,truepi,truedef,truescale,truet, numiter,skip,burnIn,Seed=1004,Save.at="results")  
#  startpi is defined by the proportion of markers that ARE associate with genes
{   # START OF FUNCTION

	  set.seed(Seed)  

	  library(msm)

	  if(numiter <=burnIn) stop ("Total Number of MCMC iterations should exceed number of burnin iterations")

	  #filename=paste("LDsimu_",Seed,"_",2,sep="")
	  #data(filename)

	  #Save.at=paste(" ",sep=" ")

	  # Parameters
	  nSNP=dim(Z)[2]  #number of SNP
	  nanim = dim(Z)[1]; #number of animals

	  # Vectors for saved samples of hyperparameters
	  if (!truedef)    {defsave   =  array(0,numiter/skip) }
	  if (!truescale)  {scalesave =  array(0,numiter/skip) }
	  if (!truepi)     {pisave    =  array(0,numiter/skip) } 
	  if (!truet)      {
	    mutsave   = array(0,numiter/skip)
	    vartsave  = array(0,numiter/skip)  
	  }
	  varesave = array(0,numiter/skip)

	  # To monitor MH acceptance ratios on degrees of freedom parameter
	  alphadef_save = array(0,(numiter-burnIn))

	  # input data
	  nrecords = length(y)
	  X = array(1,nrecords)
	  dimX = 1
	  W = as.matrix(cbind(X,Z))
	  rm(Z)
	  Wstacked=as.vector(W)
	  M = W[,-1]
	  Mstacked=as.vector(M)

	  # specify prior degrees of belief on genetic variance
	  # specify  prior scale on genetic variance
	  # inital values   

	  def=startdf			 # Starting Bayes A df 
	  scalea  = startscale    	 # Starting scale parameter
	  pi = startpi               # Starting pi parameter   (probability of being a gene!)
	  mut = 0
	  vart = 0.5
	  cdef       = 0.5		 # cdef is scale parameter for proposal density on df
	  alphacheck = 0
	  alphaQTLvar= 0			 # to monitor BayesB QTLvar rejection rates 
	  alphatot1= 0 
	  WWdiag     = apply(W,2,crossprod) # diagonals are important

	  # prior for residual variance
	  nu_e     = -1          # i.e. corresponding to Gelman's prior.
	  scale_e  = 0

	  # prior for mean of association parameters
	  mu_m     = 0
	  sigma2_m = 0.01

	  # used to monitor Metropolis Hastings acceptance ratio for df
	  dimW     = dim(W)[2]  # number of columns = # of location parameters

	  # initialize a few arrays
	  theta      = array(0,dimW)
	  SNPeff     = array(0,nSNP)
	  assoct     = array(0,nSNP)
	   SNPg      = array(0,nSNP) 
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
	  Tlast= array(0,nSNP)
	  Tcurrent= array(0,nSNP)

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
	     # sample variance & effect for each SNP	
	     ####################################################################################
	     if (pi == 1)  
	     { # "START" OF anteBAYESA
	       #####################################################################################
	       # sample conditional variance for each SNP (anteBayes A)	
	       #####################################################################################
	       varq = (scalea*def + SNPeff*SNPeff)/rchisq(nSNP,def+1)  # joint sample

	       #####################################################################################
	       # sample conditional effect for each SNP (anteBayes A)	
	       #####################################################################################
	        startSNP = proc.time()
			BayesC<- .Call("anteBayesAC",assoct,nSNP,dimX,nanim,Wstacked,WWdiag, theta, ycorr,varq,vare,SNPeff,SNPg,mut,vart)
			theta=BayesC[[1]]
			ycorr=BayesC[[2]]
			varq=BayesC[[3]]
			vare=BayesC[[4]]
			SNPeff=BayesC[[5]]
			SNPg=BayesC[[6]]
			assoct=BayesC[[7]]
	      } # "END" FOR anteBAYESA

	     #####################################################################################
	     # sample effect for each SNP (anteBayes B)	
	     #####################################################################################
	      if (pi < 1 )  
	      {   # "START" OF anteBAYESB
	         startSNP = proc.time()
	         G1=rep(0,2)	 
		  #####################################################################################
	         anteBayesB<-.Call("anteBayesBC",assoct,nSNP,dimX,nanim,Wstacked,Mstacked,WWdiag, theta,ycorr,varq,vare,SNPeff,SNPg,postprob,scalea,def,alphametrovar,pi,G1,mut,vart) 
			theta=anteBayesB[[1]]
			ycorr=anteBayesB[[2]]
			varq=anteBayesB[[3]]
			vare=anteBayesB[[4]]
			SNPeff=anteBayesB[[5]]
			postprob=anteBayesB[[6]]
			alphametrovar=anteBayesB[[7]]
			G1=anteBayesB[[8]][1] #add G1
			def=anteBayesB[[9]]
			scalea=anteBayesB[[10]]
			pi=anteBayesB[[11]]
			alphacheck=anteBayesB[[12]][1]
			SNPg=anteBayesB[[13]]
			assoct=anteBayesB[[14]] 
	     } # "END" OF BAYESB

	     if (itersave>1) 
	     {     # "START" OF COMPUTE POSTERIOR MEANS & VARIANCES
	           Mcurrent = Mlast + (theta-Mlast)/itersave
	           Qcurrent = Qlast + (itersave-1)*((theta-Mlast)^2)/itersave
	           Mlast = Mcurrent
	           Qlast = Qcurrent 

	           SNPvar=SNPvarlast + (varq-SNPvarlast)/itersave
	           SNPvarlast=SNPvar

	           Tcurrent=Tlast+ (assoct-Tlast)/itersave
	           Tlast=Tcurrent
	     }  # "END" OF COMPUTE POSTERIOR MEANS & VARIANCES
	     if (itersave==1) {Mlast = theta;Tlast= assoct;SNPvarlast=varq;} 

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
	       if ((cycle%%10 == 0) & (iter <= burnIn))
	       {
	         alphacheck = alphatot1/10;
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
	     #  based on Gelman Uniform(0,A) prior on scale parameter for "large" A.
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

	      #####################################################################################
	      #  sample mean and variance for association parameters
	      #####################################################################################
	      if (!truet)
	      {
	      ######sample mut#######
	      sigma2hat_t=1/(1/sigma2_m+length(which(assoct!=0))/vart) 
	      muhat_t=((mean(assoct[which(assoct!=0)])*length(which(assoct!=0))/vart)+(mu_m/sigma2_m))*sigma2hat_t  
	      mut = rnorm(1,mean=muhat_t,sd=sqrt(sigma2hat_t))   
     
	      ########sample vart#######################
	      vart = ( t(assoct[which(assoct!=0)]-mut)%*%(assoct[which(assoct!=0)]-mut)+1)/rchisq(1,(length(assoct[which(assoct!=0)])-1)+3)
	      }

	      ###########################################################################################
	      #store MCMC samples at every "skip"
	      ###########################################################################################
	      if (iter%%skip == 0) 
	      {
	            varesave[iter/skip] = vare
	            if(!truedef) {defsave[iter/skip] = def}
	            if(!truescale) {scalesave[iter/skip] = scalea}
	            if(!truepi) {pisave[iter/skip] = pi}
	            if(!truet){mutsave[iter/skip]=mut;vartsave[iter/skip]=vart}
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
	       if (!truet){hyperparameters = cbind(hyperparameters,mutsave); hyperparameters = cbind(hyperparameters,vartsave) }
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
	       meanmu= Mcurrent[dimX]
	       meang= Mcurrent[(dimX+1):dimW]
	       varg = Qcurrent[(dimX+1):dimW]/itersave
	       meant= Tcurrent[1:(nSNP-1)]
	       save(iter,meanmu,meang,varg,SNPvar,meant,file = paste(Save.at,"EffectsResults",Seed,".RData",sep=""))

	       #  monitor MH acceptance rates over cycle for the degrees of freedom term.
	       save(iter,alphadef_save,file=paste(Save.at, "alphadef",Seed,".RData",sep=""))

	       } # "END" OF PROCESSING MCMC SAMPLING

	  } # "END" OF MCMC SAMPLING

}
