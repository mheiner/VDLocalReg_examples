for ii in {1..100..1}
do
  for missType in MCAR MAR MNAR
  do
	  for dataType in Additive Product
	  do
	    for modtype in Mean Reg
      do
        for puse in 2
        do
          for alph in 1.0
          do
	          for sigupper in 1.0
	          do
	            for sim_type in NNiChisq_indep
	            do
	              for s0 in 0.2 # 0.1 0.25
	              do
	                for dte in 230829
	                do
		    echo "Rscript --no-restore 1_fitPPMxR.R $missType $dataType $modtype $puse $alph $sigupper $sim_type $s0 $ii $dte &> progress/fitPPMxR_mist$missType\_dtype$dataType\_modtype$modtype\_p$puse\_alph$alph\_sigupper$sigupper\_simtype$sim_type\_s0$s0\_ii$ii\_date$dte.txt"
		              done
		            done
		          done
		        done
		      done
		    done
		  done
		done
	done
done | parallel --jobs 112
wait
