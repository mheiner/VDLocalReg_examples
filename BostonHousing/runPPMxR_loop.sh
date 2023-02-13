for modtype in Mean Reg
do
  for pmiss in 0 10 25 50
  do
    for alph in 3.0
    do
	    for sigupper in 0.4
	    do
	      for sim_type in NNiChisq_indep
	      do
	        for s0 in 0.1
	        do
	          for ii in {1..100..1}
	          do
	            for dte in 220813
	            do
		            echo "Rscript --no-restore 1_fitPPMxR.R $modtype $pmiss $alph $sigupper $sim_type $s0 $ii $dte &> progress/fitPPMxR_modtype$modtype\_pMiss$pmiss\_alph$alph\_sigupper$sigupper\_simtype$sim_type\_s0$s0\_ii$ii\_date$dte.txt"
		          done
		        done
		      done
		    done
		  done
	  done
	done
done | parallel --jobs 112 # if using GNU Parallel

wait