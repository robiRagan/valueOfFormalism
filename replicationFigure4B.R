#' replicationFigure4B.R
#'######################## 
#' #' This file allows a user to evaluate the code used in the simulations found in:
#' 
#' Dougherty, Edward and Ragan (Forthcoming) "The Value of Formalism".
#' http://link.springer.com/article/10.1007/s11127-014-0191-1
#' 
#' There is an accompaning R that allows a user to run versions of the model not displayed in the paper:
#' 
#' generalCodeAllFunctions.R
#'         
#'  All the versions contain all of the code used in the paper.
#'  
#'  
#'  To use this file:
#'  
#'  1. Run all the code at once.
#'   
#'   ----------------------------------------------------------------------
#'   Copyright (C) 2014  Robi Ragan: robi.ragan@gmail.com
#'   This program is free software: you can redistribute it 
#'   and/or modify it under the terms of the GNU General Public License 
#'   as published by the Free Software Foundation, either version 3 of the License.
#'   
#'   This program is distributed in the hope that it will be useful, 
#'   but WITHOUT ANY WARRANTY; without even the implied warranty of
#'   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#'   GNU General Public License for more details.
#'   
#'   You should have received a copy of the GNU General Public License
#'   along with this program.  If not, see <http://www.gnu.org/licenses/>.
#'   ----------------------------------------------------------------------  

rm(list = ls(all = TRUE)) # clear the workspace



#--------------------------------------------------------------------------------------------------------------------------
# DIRECTORY OF FUNCTIONS: This is a directory of all the functions defined in this R script. More details about each function can be found directly above the code that defines the function.
#
#   # Distributions #
#   #--------------# 
# - drawNormalValues(): Draws utility from a N(groupMean, standardDeviation).
#
#   # Generating Inital DataFrames #
#   #------------------------------#
# - genGroupLevelDataFrame(): Creates the characteristics of the subgroups that the voters can be a part of.
# - genVoterLevelDataFrame(): Takes the output of genGroupLevelDataframe() and draws ui and ei for the voters using the drawNormalValues() function.
# - updateVoterLevelDataFrame(): Takes an existing voter level data frame and a group level data frame that has had it's parameters changed and outputs a new voter level data frame.
#
#   # Modeling Functions #
#   #-------------------#
# seriesOfProposals(): Runs the one "sucessive procedure" as described above, for all k-majority rules.
# iterations(): Runs numberOfIterations of the model. Stores all of the input parameters, generated values and output for further analysis.
# plotNumberOfRounds(): Plots the mean number of rounds it took for the status quo to be defeated for each k-majority rule. It is the average value across all iterations.
# plotDecisionCost: Plots decison cost for each k-majority rule. It is the average value across all the iterations.
# plotOnlyExternalCost: Plots the external costs metrics for each of the kMajority rules. It is the average value across all iterations.
# plotExternalDecisionTotalCost: Plots the total costs metrics for each of the kMajority rules. It is the average value across all iterations.
# aKMajorityCostMultiSimulation(): Runs an entire simulation of the model based on parameters supplied by the user.
############################################################################################################################


#-----------------------------------------------------------------------------------------------------------
#' drawNormalValues
#' Draws ui or ei values from N(groupMean, groupStandardDeviation).
#' @param sizeOfDraw The number of elements that need to be drawn.
#' @param groupMean The mean of the normal distribution used to draw ui or ei for a group.
#' @param groupStandardDeviation The standard deviation of the normal distributuion used to draw ui or ei for a group.

drawNormalValues <- function(sizeOfDraw,groupMean,groupStandardDeviation){
  if (groupStandardDeviation < 0){stop("The standard deviation provided for the NORMAL distribution is: ",groupStandardDeviation,".\n  The standard deviation must be >= 0")}
#  set.seed(100) # Keep off except for testing
  oneGroupValues <-rnorm(sizeOfDraw,groupMean,groupStandardDeviation) # Takes the random sample from a normal.
} # ends the function


#-----------------------------------------------------------------------------------------------------------
#' genGroupLevelDataframe
#' Creates a data.frame to hold the group level data for the simulation.
#' @param groupSize: A vector of length G (the number of subgroups). Each element, indicates the number of voters in each group.
#' @param utilityDistribution: "normal" is currently the only allowed distribution.
#' @param utilityDistributionParameters: The relevant parameters needed for the distribution provided as utilityDistribution. A pair of parameters is provided for each group. For utilityDistribution="normal", this is the mean and standard deviation. For example for 5 groups and utilityDistribution="normal", you might provide c(-.7,.2,-.1,.2,0,.2,-.3,.2,.2,.2).
#' @param errorDistribution: The distribution the error ei values will be drawn from. This can be "uniform", "normal", or "beta".
#' @param errorDistributionParameters: The relevant parameters needed for the distribution provided as errorDistribution.
#' @param groupPostFailingProposalMeanUiIncrease: The amount the mean used to sample a group's ui goes up after a failing proposal. Must be entered as a string vector.
#' @return groupsDataFrame: A dataframe with G rows, containing the following columns: groupID, utilityDistribution, utilityDistributionParam1, utilityDistributionParam2, errorDistribution, errorDistributionParam1, errorDistributionParam2, groupPostFailingProposalMeanUiIncrease. 

genGroupLevelDataFrame <- function(groupSize,utilityDistribution,utilityDistributionParameters,errorDistribution,errorDistributionParameters,groupPostFailingProposalMeanUiIncrease){
  numberOfGroups <- length(groupSize) 
  if (numberOfGroups*2 != length(utilityDistributionParameters)){stop("The provided parameter `utilityDistributionParameters` should contain ", numberOfGroups*2," elements.\n  You provided ",length(utilityDistributionParameters)," elements.")}
  if (numberOfGroups*2 != length(errorDistributionParameters)){stop("The provided parameter `errorDistributionParameters` should contain ", numberOfGroups*2," elements.\n  You provided ",length(errorDistributionParameters)," elements.")}
  if (numberOfGroups != length(groupPostFailingProposalMeanUiIncrease)){stop("The provided parameter `groupPostFailingProposalMeanUiIncrease` should contain ", numberOfGroups," elements.\n  You provided ",length(groupPostFailingProposalMeanUiIncrease)," elements.")}
      
      utilityDistributionParam1 <- matrix(utilityDistributionParameters, nrow=numberOfGroups, ncol=2, byrow=TRUE)[,1]
      utilityDistributionParam2 <- matrix(utilityDistributionParameters, nrow=numberOfGroups, ncol=2, byrow=TRUE)[,2]
      errorDistributionParam1 <- matrix(errorDistributionParameters, nrow=numberOfGroups, ncol=2, byrow=TRUE)[,1]
      errorDistributionParam2 <- matrix(errorDistributionParameters, nrow=numberOfGroups, ncol=2, byrow=TRUE)[,2]
  
groupDataFrame <- data.frame(groupID=seq(1:numberOfGroups),groupSize=groupSize,utilityDistribution=utilityDistribution, utilityDistributionParam1=utilityDistributionParam1, utilityDistributionParam2=utilityDistributionParam2,errorDistribution=errorDistribution, errorDistributionParam1=errorDistributionParam1, errorDistributionParam2=errorDistributionParam2,groupPostFailingProposalMeanUiIncrease=groupPostFailingProposalMeanUiIncrease)

groupDataFrame
}


#---------------------------------------------------------------------------------------------------------------
#' genVoterLevelDataframe
#' Takes the information from a group level data frame generated by genGroupLevelDataframe(), and draws ui and ei for the groups of voters based on it.
#' @param groupDataFrame: A data frame generated by genGroupLevelDataframe.
#' @return voterDataFrame: A data frame with a row for each, containing the following columns: groupID, ui and ei.
genVoterLevelDataFrame<- function(groupDataFrame){
  numberOfGroups <- nrow(groupDataFrame)          # Extracts the number of groups from the group data frame.
  numberOfVoters <- sum(groupDataFrame$groupSize) # Calculates the total number of voters from the group data frame.
  # Create Data Frame and Assign Group Numbers
  groupIDnumbers <- vector(mode = "numeric", length = 0) # Empty vector to put ID numbers in
  for (i in 1:numberOfGroups){
    groupIDnumbers <- c(groupIDnumbers,rep(i,groupDataFrame$groupSize[i]))
    voterDataFrame <- data.frame(groupID=factor(groupIDnumbers))
  }
  for (i in 1:numberOfGroups){    # This loop draws ui values
      aGroup <- subset(voterDataFrame,groupID==i)                    # First subset the data frame for group 
      sizeOfAGroup <- nrow(aGroup)                                   # determine the number of voters
      aGroupUtility <- drawNormalValues(groupDataFrame$groupSize[i],groupDataFrame$utilityDistributionParam1[i],groupDataFrame$utilityDistributionParam2[i])    # Draw the values from the normal.
      voterDataFrame$ui[voterDataFrame$groupID==i] <- aGroupUtility # Now place these utilities in the data frame
  } # Ends the ui draw loop 
  for (i in 1:numberOfGroups){                            # This loop draws ei values
    aGroup <- subset(voterDataFrame,groupID==i)           # First subset the data frame for group 
    sizeOfAGroup <- nrow(aGroup)                          # determine the number of voters
        if (groupDataFrame$errorDistribution[i]=="normal"){                      # IF "normal" check.
          aGroupError <- drawNormalValues(groupDataFrame$groupSize[i],groupDataFrame$errorDistributionParam1[i],groupDataFrame$errorDistributionParam2[i])    # Draw the values from the normal()
        }
    voterDataFrame$ei[voterDataFrame$groupID==i] <- aGroupError # Now place these utilities in the data frame
  } # Ends the ei draw loop
  voterDataFrame
}  



#--------------------------------------------------------------------------------------------------------------
#' updateVoterLevelDataFrame
#' Takes an existing voter level data frame and a group level data frame that has had it's parameters changed and outputs a new voter level data frame.
#' @param oneIterationsVoters: The existing voter level data frame that will be updated.
#' @param oneIterarionsGroups: The updated group level data frame that provides the values used to update the voter level data frame.
#' @return oneIterationsVoters: The updated voter level data frame.
updateVoterLevelDataFrame <- function(oneIterationsVoters,oneIterationsGroups){
  numberOfGroups <- nrow(oneIterationsGroups)
  for (i in 1:numberOfGroups){    # This loop draws ui values
    aGroup <- subset(oneIterationsVoters,groupID==i)                      # First subset the data frame for group 
    sizeOfAGroup <- nrow(aGroup)                                   # determine the number of voters
    aGroupUtility <- drawNormalValues(oneIterationsGroups$groupSize[i],oneIterationsGroups$utilityDistributionParam1[i],oneIterationsGroups$utilityDistributionParam2[i])    # Draw the values from the normal.
    oneIterationsVoters$ui[oneIterationsVoters$groupID==i] <- aGroupUtility # Now place these utilities in the data frame
  } # Ends the ui draw loop 
  oneIterationsVoters
}


#' ------------------------------------------------------------------------------------------------------------
#' seriesOfProposals()
#' Runs a series of votes/multiple alternatives following a follows a “successive” voting procedure. Stores all of the input parameters, generated values and output for further analysis.
#' In a successive procedure, the initial status quo q1 is paired against a proposal x1 in round 1. If x1 passes, voting ends. If x1 fails, q1 is paired against x2 in round 2, and so on for a total of R rounds (see Figure 5 in the paper). In a standard successive procedure, voting continues until the proposal passes or a speciﰀc round R is reached and the proposal either passes or fails. In our study, voting continues until the proposal eventually passes, which we assure by improving the popularity of proposals in successive rounds. We also examine a few cases where some individuals are made better off and others are made worse off as the series proceeds. Continually improving proposals can be modeled in several ways. We assume that the expected utility of a group increases by an increment, groupPostFailingProposalMeanUiIncrease (alpha_gr in the paper), each round a proposal does not pass. The fnal round, R may result from a limitation in the number of proposals or a time limit, etc. To keep the analysis of decision costs simple, we assume each round imposes the same decision costs on all members of the assembly, c > 0. Voting rules which require many rounds to pass a proposal will generate more decision costs than voting rules which require few rounds.
#' @param oneIterationsVoters: A dataframe created by genVoterLevelDataFrame(). Each row represents a voter and the columns are groupID, ui, ei.
#' @param oneIterationsGroups: A dataframe created by genGroupLevelDataFrame. Each row represents a group and the columns are groupID, ui and ei.
#' @param maximumNumberOfProposalsInASeries: An integer or FALSE. If an integer, it represents the maximum possible number of proposals considered by a group of voters. Note though, that even if if maximumNumberOfProposalsInASeries is set to an integer, that voting will still stop if a proposal passes for all k-majority rules being considerd. To allow the series of proposals to anly stop once all k-majority Rules have passes a proposal, set maximumNumberOfProposalsInASeries=FALSE. 
#' @param silentSeries: FALSE prints notifications to the console as each series finishes. TRUE silences the notifications.
#' @param iterationNumber: the current iteration number taken from iterations().
#' @return A list of objects: votersValuesFromEachRound, yeasEachRound, passesForWhichKMajorityEachRound, roundTheProposalPassed, numberOfProposalsConsidered, typicalVotersExternalCostOut, bestOffGroupsMeanExternalCostOut, worstOffGroupsMeanExternalCostOut)

seriesOfProposals <- function(oneIterationsVoters, oneIterationsGroups, maximumNumberOfProposalsInASeries, silentSeries, iterationNumber){
  numberOfVoters <- nrow(oneIterationsVoters)
  # The series storage containers that will store the user input, generated values and the output.
  votersValuesFromEachRound <- list()
  yeasEachRound <- list()
  passesForWhichKMajorityEachRound <- list()
  typicalVotersTotalWelfareEachRound <- list()
  typicalVotersExternalCostEachRound <- list()
  bestOffGroupsMeanExternalCostEachRound <- list()
  worstOffGroupsMeanExternalCostEachRound <- list()
  proposalParetoPreferedEachRound <- list()
  proposalFailsForWhichKMajorityEachRound <- list()
  kMajoritiesThatFailedToPassParetoPrefEachRound <- list()
  passesForWhichKMaj <- rep(0,numberOfVoters)
  passedBeforeThisRoundForThisK <- rep(0,numberOfVoters)
  
  r <- 0 # round counter
  while(min(passesForWhichKMaj)==0 & (maximumNumberOfProposalsInASeries==FALSE | r<maximumNumberOfProposalsInASeries)){ # Begins the series of proposals loop
    r <- r+1
    # STEP 1: Calculate the utility plus error for each voter, and welfare loss and store in the dataframe.
    oneIterationsVoters$utilityWithError <- rowSums(cbind(oneIterationsVoters$ui,oneIterationsVoters$ei))
    oneIterationsVoters$externalCost <- ifelse(oneIterationsVoters$ui > 0,0,-oneIterationsVoters$ui) 
    # STEP 2: Probability of Voting: The probability a voter votes yes is recorded as 1 if their utility with error is > 0. It is recorded as zero if utility with error is <= to 0.  This proabability is recorded in the data frame for each set of voters.
    oneIterationsVoters$probabilityVoteYea <- ifelse(oneIterationsVoters$utilityWithError>0,1,0)
    # STEP 3: Sum up the probabilities.
    numberYeasThisRound <- sum(oneIterationsVoters$probabilityVoteYea)
    # STEP 4: Determine if there is a simple majority for each k-majority this round, or that a proposal passed before this round.
    passesForWhichKMaj <- ifelse(((numberYeasThisRound >= c(1:nrow(oneIterationsVoters))) | passedBeforeThisRoundForThisK==1),1,0)
    # STEP 5: Find the mean of each group's external cost.
      # tapply() runs the mean() function on the ui, for each groupID. 
      # unlist() simply turns the list returned by tapply() into a simple vector.
    groupExternalCostMeans <- unlist(tapply(oneIterationsVoters$externalCost, oneIterationsVoters$groupID, mean, simplify=FALSE))

    # STEP 6: Calculate the relavent expected cost measures for each k-majority: 
    # The external cost of each voter is calculated in the following way.
    # If a proposal passes at the kMajority:
      #   - Yea voters have no loss.
      #   - Nay voters recieve a loss equal |u_i|.
    # If a proposal fails at the kMajority:
      #   - Yea voters have no loss.
      #   - Nay voters recieve have no loss.
  # The expected cost is calculated and stored for each of the following voters.
    #   - typical voter (mean of all ui)
    #   - highest group mean's mean.
    #   - lowest group mean's mean.
    meanVoterExternalCost <- ifelse(passesForWhichKMaj==1, mean(oneIterationsVoters$externalCost),0) # the mean voter's utility.
    bestOffGroupsMeanExternalCost <- ifelse(passesForWhichKMaj==1, min(groupExternalCostMeans),0) # For k-majority Rules that passed a proposal, store the mean EC for the best off group.
    worstOffGroupsMeanExternalCost <- ifelse(passesForWhichKMaj==1, max(groupExternalCostMeans),0) # For k-majority Rules that passed a proposal, store the mean EC for the worst off group.
    
    # STEP 9
    # Record the number of rounds in which a Pareto prefered proposal was defeated.
      # determine if preferences indicate the proposal is Pareto prefered in each round.
      proposalParetoPrefered <-ifelse(sum(oneIterationsVoters$ui>0)==numberOfVoters,1,0) # Check if everyone has positive utility. If so the proposal is Pareto Preffered to the SQ
      # determine if the proposal failed
      proposalFailsForWhichKMajority <- ifelse(passesForWhichKMaj==0, 1, 0)
      # determine if proposal failed for a k-majority and was Pareto preferred.
      paretoPreferedFailedThisKMaj <- ifelse(proposalParetoPrefered==1 & passesForWhichKMaj==0,1,0)

  
    # STEP 10: Store the output from the rth round in the series.
    votersValuesFromEachRound[[r]] <- oneIterationsVoters
    yeasEachRound[[r]] <- numberYeasThisRound                                  # Store the number of yeas this round.
    passesForWhichKMajorityEachRound[[r]] <- passesForWhichKMaj      # Store which kMajority the proposal passes for in this iteration.
    proposalFailsForWhichKMajorityEachRound[[r]] <- proposalFailsForWhichKMajority
    proposalParetoPreferedEachRound[[r]] <- proposalParetoPrefered
    typicalVotersExternalCostEachRound[[r]] <- meanVoterExternalCost              # Store the mean voter's total welfare.
    bestOffGroupsMeanExternalCostEachRound[[r]] <- bestOffGroupsMeanExternalCost # Store the highest group mean.
    worstOffGroupsMeanExternalCostEachRound[[r]] <- worstOffGroupsMeanExternalCost  # Store the lowest group mean.
    kMajoritiesThatFailedToPassParetoPrefEachRound[[r]] <- paretoPreferedFailedThisKMaj # Store which kMajorities failed to pass pareto prefered proposals.
    
    if (silentSeries==FALSE){
    cat("   [Iteration ",iterationNumber,"]  (The vote on the proposal in round ",r, " is complete. kMajorities that passed proposal: ",sum(passesForWhichKMaj),")\n", sep = "") # A counter that prints to the console.
    }
  

    # STEP 11 add the groupPostFailingProposalMeanUiIncrease to the mean used to draw the voters in the previous round.
    for(g in 1:nrow(oneIterationsGroups)){
    aGroupsIncrease <- as.vector(oneIterationsGroups$groupPostFailingProposalMeanUiIncrease[g])
    oneIterationsGroups$utilityDistributionParam1[g] <- oneIterationsGroups$utilityDistributionParam1[g] + eval(parse(text=aGroupsIncrease))
    }
    

    # STEP 12
    # Update the voters data frame with new utility values based on the increased group means
   oneIterationsVoters <- updateVoterLevelDataFrame(oneIterationsVoters,oneIterationsGroups)
    



    # STEP 13
    # Update the passedBeforeThisRoundForThisK indicator vector for use in the next round/loop.
    passedBeforeThisRoundForThisK <- ifelse(passesForWhichKMaj==1 | passedBeforeThisRoundForThisK==1,1,0)
      
  } # Ends the series of proposals loop ###
   

  
  # Reshape some of the lists entries into the most simple object possible.
    yeasEachRoundOut <- as.vector(unlist(yeasEachRound))              
    passesForWhichKMajorityEachRoundOut <- matrix(unlist(passesForWhichKMajorityEachRound),r,numberOfVoters,byrow=TRUE)
    typicalVotersExternalCostEachRoundAll <- matrix(unlist(typicalVotersExternalCostEachRound),r,numberOfVoters,byrow=TRUE)            
    bestOffGroupsMeanExternalCostEachRoundAll <-  matrix(unlist(bestOffGroupsMeanExternalCostEachRound),r,numberOfVoters,byrow=TRUE) 
    worstOffGroupsMeanExternalCostEachRoundAll <-  matrix(unlist(worstOffGroupsMeanExternalCostEachRound),r,numberOfVoters,byrow=TRUE)
    kMajoritiesThatFailedToPassParetoPrefEachRoundOut <-  matrix(unlist(kMajoritiesThatFailedToPassParetoPrefEachRound),r,numberOfVoters,byrow=TRUE)
  # Find the first round a proposal passed for each k-majority rule
    roundTheProposalPassed <- (nrow(passesForWhichKMajorityEachRoundOut)+1)-colSums(passesForWhichKMajorityEachRoundOut)
    # Correct roundTheProposalPassed for kMajorities that did not pass a proposal
      roundTheProposalPassed <- ifelse(roundTheProposalPassed>nrow(passesForWhichKMajorityEachRoundOut),0,roundTheProposalPassed)
  # Calculate the number of proposals considered.
    numberOfProposalsConsidered <- (nrow(passesForWhichKMajorityEachRoundOut)+1)-colSums(passesForWhichKMajorityEachRoundOut)
    # Correct for k-majorities that did not pass a proposal.
      numberOfProposalsConsidered <- ifelse(numberOfProposalsConsidered > r, r, numberOfProposalsConsidered)
  
  # Now pull out the external cost measures when the proposal passed for each k-majority
  typicalVotersExternalCostOut <- rep(NA,numberOfVoters)
  for (w in 1:numberOfVoters){
    if(roundTheProposalPassed[w]==0){
      typicalVotersExternalCostOut[w] <- 0
    } else {
      typicalVotersExternalCostOut[w] <- typicalVotersExternalCostEachRoundAll[roundTheProposalPassed[w],w]
    }
  }
  
  bestOffGroupsMeanExternalCostOut <- rep(NA,numberOfVoters)
  for (w in 1:numberOfVoters){
    if(roundTheProposalPassed[w]==0){
      bestOffGroupsMeanExternalCostOut[w] <- 0
    } else {
      bestOffGroupsMeanExternalCostOut[w] <- bestOffGroupsMeanExternalCostEachRoundAll[roundTheProposalPassed[w],w]
    }
  }
  
   worstOffGroupsMeanExternalCostOut <- rep(NA,numberOfVoters)
  for (w in 1:numberOfVoters){
    if(roundTheProposalPassed[w]==0){
      worstOffGroupsMeanExternalCostOut[w] <- 0
    } else {
      worstOffGroupsMeanExternalCostOut[w] <- worstOffGroupsMeanExternalCostEachRoundAll[roundTheProposalPassed[w],w]
    }
  }

# Output the values  
list(votersValuesFromEachRound=votersValuesFromEachRound, yeasEachRound=yeasEachRoundOut, passesForWhichKMajorityEachRound=passesForWhichKMajorityEachRoundOut, roundTheProposalPassed=roundTheProposalPassed, numberOfProposalsConsidered= numberOfProposalsConsidered, typicalVotersExternalCostOut=typicalVotersExternalCostOut, bestOffGroupsMeanExternalCostOut=bestOffGroupsMeanExternalCostOut, worstOffGroupsMeanExternalCostOut=worstOffGroupsMeanExternalCostOut, kMajoritiesThatFailedToPassParetoPrefEachRoundOut=kMajoritiesThatFailedToPassParetoPrefEachRoundOut)
  
}# Ends seriesOfProposals() Function                                                             




#------------------------------------------------------------------------------------------------------------
#' iterations
#' Runs numberOfIterations of seriesOfProposals() with the same set of initial parameters. Stores all of the input parameters, generated values and output for further analysis.
#' 
#' @param numberOfIterations: The number of iterations the model is run.
#' @param perProposalDecisionCost: The decision cost incured for each proposal a group considers.
#' @param groupSize: A vector of length G (the number of subgroups). Each element, indicates the number of voters in each group.
#' @param utilityDistribution: "normal" is currently the only allowed distribution.
#' @param utilityDistributionParameters: The relevant parameters needed for the distribution provided as utilityDistribution. A pair of parameters is provided for each group. For utilityDistribution="normal", this is the mean and standard deviation. For example for 5 groups and utilityDistribution="normal", you might provide c(-.7,.2,-.1,.2,0,.2,-.3,.2,.2,.2).
#' @param errorDistribution: The distribution the error ei values will be drawn from. This can be "uniform", "normal", or "beta".
#' @param errorDistributionParameters: The relevant parameters needed for the distribution provided as errorDistribution.
#' @param groupPostFailingProposalMeanUiIncrease: The amount the mean used to sample a group's ui goes up after a failing proposal. Must be entered as a string vector.
#' @param maximumNumberOfProposalsInASeries: An integer or FALSE. If an integer, it represents the maximum possible number of proposals considered by a group of voters. Note though, that even if if maximumNumberOfProposalsInASeries is set to an integer, that voting will still stop if a proposal passes for all k-majority rules being considerd. To allow the series of proposals to anly stop once all k-majority Rules have passes a proposal, set maximumNumberOfProposalsInASeries=FALSE. 
#' @param silentSeries: FALSE prints notifications to the console as each series finishes. TRUE silences the notifications.
#' @return A list of objects: expectedCosts, rounds, allGroups, allVoters, allYeas, allPassesForWhich, theInputParameters.
            
iterations <- function( numberOfIterations,          # number of iterations of the mode run.
                        groupSize,                 
                        utilityDistribution,    
                        utilityDistributionParameters,
                        errorDistribution,
                        errorDistributionParameters,
                        groupPostFailingProposalMeanUiIncrease,
                        maximumNumberOfProposalsInASeries,
                        perProposalDecisionCost,
                        silentSeries,
                        silentIterations){

numberOfVoters <- sum(groupSize)
# The iterations storage containers that will store the user input, generated values and the output.
inputParameters <- list(numberOfIterations=numberOfIterations,groupSize=groupSize,utilityDistribution=utilityDistribution,utilityDistributionParameters=utilityDistributionParameters,errorDistribution=errorDistribution,errorDistributionParameters=errorDistributionParameters,groupPostFailingProposalMeanUiIncrease=groupPostFailingProposalMeanUiIncrease, maximumNumberOfProposalsInASeries=maximumNumberOfProposalsInASeries,perProposalDecisionCost=perProposalDecisionCost)

votersFromEachIteration <- vector(mode="list", length=numberOfIterations)
groupsFromEachIteration <- vector(mode="list", length=numberOfIterations)
yeasEachRoundEachIteration <-  list()
passesForWhichKMajorityEachIteration <- list()
kMajorityFailedToPassParetoPrefByRoundEachIteration <- list()
numberOfProposalsConsideredEachIteration <- matrix(NA,numberOfIterations,numberOfVoters)
roundProposalPassedEachIteration <- matrix(NA,numberOfIterations,numberOfVoters)
typicalVotersExternalCostEachIteration <- matrix(NA,numberOfIterations,numberOfVoters)
bestOffGroupsMeanExternalCostEachIteration <- matrix(NA,numberOfIterations,numberOfVoters)
worstOffGroupsMeanExternalCostEachIteration <- matrix(NA,numberOfIterations,numberOfVoters)

for (j in 1:numberOfIterations){ # Begins the iterations loop
    # STEP 1: Create the groups for the voters to be a part of.
    oneIterationsGroups <- genGroupLevelDataFrame(groupSize=groupSize,utilityDistribution=utilityDistribution,utilityDistributionParameters=utilityDistributionParameters,errorDistribution=errorDistribution,errorDistributionParameters=errorDistributionParameters,groupPostFailingProposalMeanUiIncrease=groupPostFailingProposalMeanUiIncrease)
    # STEP 2: Generate the voters with ui and ei
    oneIterationsVoters <- genVoterLevelDataFrame(groupDataFrame=oneIterationsGroups)
    # STEP 3: Run the series of votes (multiple rounds of voting)
    seriesOutput <- seriesOfProposals(oneIterationsVoters=oneIterationsVoters,oneIterationsGroups=oneIterationsGroups,maximumNumberOfProposalsInASeries=maximumNumberOfProposalsInASeries,silentSeries=silentSeries,iterationNumber=j)
    
    # STEP 4: Store the output from the jth iteration.
    votersFromEachIteration[[j]] <- oneIterationsVoters                 # Store the voters dataframe.
    
    groupsFromEachIteration[[j]] <- oneIterationsGroups                 # Store the groups dataframe.
    
    yeasEachRoundEachIteration[[j]] <- seriesOutput$yeasEachRound                                  # Store the number of yeas.
    passesForWhichKMajorityEachIteration[[j]] <- seriesOutput$passesForWhichKMajorityEachRound       # Store which kMajority the proposal passes for in this iteration.
    kMajorityFailedToPassParetoPrefByRoundEachIteration[[j]] <- seriesOutput$kMajoritiesThatFailedToPassParetoPrefEachRound
    roundProposalPassedEachIteration[j,] <- seriesOutput$roundTheProposalPassed # Store the rounds that the proposals passed for an iteration.
    numberOfProposalsConsideredEachIteration[j,] <- seriesOutput$numberOfProposalsConsidered # Store the rounds that the proposals passed for an iteration.
    typicalVotersExternalCostEachIteration[j,] <- seriesOutput$typicalVotersExternalCostOut              # Store the mean voter's total welfare.
    bestOffGroupsMeanExternalCostEachIteration[j,] <- seriesOutput$bestOffGroupsMeanExternalCostOut # Store the highest group mean.
    worstOffGroupsMeanExternalCostEachIteration[j,] <- seriesOutput$worstOffGroupsMeanExternalCostOut  # Store the lowest group mean.
    
    if (silentIterations==FALSE){    
    cat("\n============================\n iteration ",j, " is complete. \n============================\n\n", sep = "")
                                }   
    } # Ends the iterations loop


# Calculate Summaries of the iterations.
meanRoundProposalPassedEachIteration <- apply(roundProposalPassedEachIteration,2,mean)
meanNumberOfProposalsConsideredEachIteration <- apply(numberOfProposalsConsideredEachIteration,2,mean)
sumOfProposalsConsideredAcrossAllIterations <- colSums(numberOfProposalsConsideredEachIteration)

numberOfRoundsAkMajoritiesFailedToPassParetoAllIterations <- t(sapply(kMajorityFailedToPassParetoPrefByRoundEachIteration,colSums))
sumOfRoundsAkMajoritiesFailedToPassParetoAcrossAllIterations <- colSums(numberOfRoundsAkMajoritiesFailedToPassParetoAllIterations)
proportionOfRoundsParetoPrefFailedAllIterations <- sumOfRoundsAkMajoritiesFailedToPassParetoAcrossAllIterations/sumOfProposalsConsideredAcrossAllIterations

meanOfMeanVotersExternalCostEachIteration <- colMeans(typicalVotersExternalCostEachIteration)
meanOfbestOffGroupsMeanExternalCostEachIteration <- colMeans(bestOffGroupsMeanExternalCostEachIteration)
meanOfworstOffGroupsMeanExternalCostEachIteration <- colMeans(worstOffGroupsMeanExternalCostEachIteration)

externalCostOutput <- data.frame(meanOfMeanVotersExternalCostEachIteration, meanOfbestOffGroupsMeanExternalCostEachIteration, meanOfworstOffGroupsMeanExternalCostEachIteration)

roundsOutput <- data.frame(meanRoundProposalPassedEachIteration=meanRoundProposalPassedEachIteration, meanNumberOfProposalsConsideredEachIteration=meanNumberOfProposalsConsideredEachIteration)

list(externalCost = externalCostOutput, rounds = roundsOutput, allGroups=groupsFromEachIteration, allVoters=votersFromEachIteration, allYeas=yeasEachRoundEachIteration, allPassesForWhich=passesForWhichKMajorityEachIteration, theInputParameters=inputParameters, proportionParetoFailures=proportionOfRoundsParetoPrefFailedAllIterations) # This outputs all of the stored objects as a list.
}   ######   Ends the iterations function




#----------------------------------------------------------------------------
#' plotNumberOfRounds
#' Plots the mean number of rounds it took for the status quo to be defeated for each k-majority rule, across all the iterations.
#' 
#' @param outputDataList: The output data list of summaries, that is generated by the iterations() function
#' @return A plot of the mean, across all iterations, that a proposal passed for each k-majority rule.
plotNumberOfRounds<- function(outputDataList){
  numberOfKMajorities <- nrow(outputDataList$rounds)
  xRange <- range(c(0:numberOfKMajorities))
  yRange <- range(c(0:max(outputDataList$rounds$meanRoundProposalPassedEachIteration)))
  plot(xRange, yRange, type="n", xlab="k-majority", ylab="Mean Number of Rounds SQ Prevailed")
  lines(c(1:numberOfKMajorities), outputDataList$rounds$meanRoundProposalPassedEachIteration, col = "Black", lwd = 2, lty = "solid")
  legend(1,max(outputDataList$rounds$meanRoundProposalPassedEachIteration), # places a legend at the appropriate place 
         c("Mean Rounds", "Min-Max Rounds Interval"), # puts text in the legend 
         lty=c("solid", "dotted"), # gives the legend appropriate symbols (lines)
         lwd=c(3,3),col=c("Black","Black")) # gives the legend lines the correct color and width
}


#------------------------------------------------------------------------------------------------------------
#' plotDecisionCost
#' Plots the mean number of rounds it took for the status quo to be defeated for each k-majority rule, across all the iterations.
#' 
#' @param outputDataList: The output data list of summaries, that is generated by the iterations() function
#' @return A plot of the mean, across all iterations, of the decision cost incurred for each k-majority rule.
plotDecisionCost<- function(outputDataList){
  numberOfKMajorities <- nrow(outputDataList$externalCost)
  roundPropPassed <- outputDataList$rounds$meanRoundProposalPassedEachIteration
  xRange <- range(c(0:numberOfKMajorities))
  yRange <- range( c(0:ceiling((max(outputDataList$rounds$meanNumberOfProposalsConsideredEachIteration)*outputDataList$theInputParameters$perProposalDecisionCost))))
  plot(xRange, yRange, type="n", xlab="k-majority", ylab="Decision Costs")
  lines(c(1:numberOfKMajorities), outputDataList$rounds$meanNumberOfProposalsConsideredEachIteration*outputDataList$theInputParameters$perProposalDecisionCost, col = "Black", lwd = 3)
}


#------------------------------------------------------------------------------------------------------------
#' plotOnlyExternalCost
#' 
#' Plots the welfare loss measures for the voters of interest, in the round the proposal passed, for each of the kMajority rules.
#' @param outputDataList: The output data list of summaries, that is generated by the iterations() function
#' @return A plot of the mean, across all iterations, of the external cost incurred for each k-majority rule.
plotOnlyExternalCost <- function(outputDataList){
  x_axislabels <- seq(0,nrow(outputDataList$externalCost),by=10) 
  y_axislabels <- seq(0,1,by=.1)
  numberOfKMajorities <- nrow(outputDataList$externalCost)
  xRange <- range(c(1:numberOfKMajorities))
  yRange <- c(0,1)
  plot(xRange, yRange, type="n", xlab="k-majority", ylab="External Cost", axes=FALSE, frame=TRUE)
  axis(side=1, at=x_axislabels)
  axis(side=2, at=y_axislabels)
  lines(c(1:numberOfKMajorities),outputDataList$externalCost$meanOfMeanVotersExternalCostEachIteration, col = "Black", lwd = 2, lty = "solid")
  lines(c(1:numberOfKMajorities),outputDataList$externalCost$meanOfworstOffGroupsMeanExternalCostEachIteration, col = "Black", lwd = 2, lty = "dotted")
  lines(c(1:numberOfKMajorities),outputDataList$externalCost$meanOfbestOffGroupsMeanExternalCostEachIteration, col = "Black", lwd = 2, lty = "dotdash")
  mtext(text=paste("Group Sizes:", paste(outputDataList$theInputParameters$groupSize, collapse=", ")),side=3,line=2.540,cex=.8,font=1)
  mtext(text=paste("Utility:", paste(toupper(substring(outputDataList$allGroups[[1]]$utilityDistribution,1,1)),"(",outputDataList$allGroups[[1]]$utilityDistributionParam1,",",outputDataList$allGroups[[1]]$utilityDistributionParam2,")", collapse=", ", sep="")),side=3,line=1.50,cex=.8,font=1)
  mtext(text=paste("Error:", paste(toupper(substring(outputDataList$allGroups[[1]]$errorDistribution,1,1)),"(",outputDataList$allGroups[[1]]$errorDistributionParam1,",",outputDataList$allGroups[[1]]$errorDistributionParam2,")", collapse=", ", sep="")),side=3,line=.5,cex=.8,font=1)
  legend(70,1, # places a legend at the appropriate place 
         c("Mean Voter","Worst Group", "Best Group"), # puts text in the legend 
         lty=c("solid", "dotted", "dotdash"), # gives the legend appropriate symbols (lines)
         lwd=c(2,2,2),col=c("Black","Black","Black")) # gives the legend lines the correct color and width
}


#------------------------------------------------------------------------------------------------------------
#' plotExternalDecisionTotalCost
#' 
#' Plots the welfare loss measures for the voters of interest, in the round the proposal passed, for each of the kMajority rules.
#' @param outputDataList: The output data list of summaries, that is generated by the iterations() function
#' @param plotMeanEC: if TRUE plot the costs for the Mean Group
#' @param plotBestEC: if TRUE plot the costs for the Best Group
#' @param plotWorstEC: if TRUE plot the costs for the Worst Group
#' @return A plot of the mean, across all iterations, of the external cost incurred for each k-majority rule.
plotExternalDecisionTotalCost <- function(outputDataList,plotMeanEC,plotBestEC,plotWorstEC){  
  
  # Set the margins so the labels in the Top Margin will display.
  # par(mar=c(5.1, 4.1, 5.1, 2.1))
  
  #Extract External Cost
    meanExternalCost <- outputDataList$externalCost$meanOfMeanVotersExternalCostEachIteration
    bestExternalCost <- outputDataList$externalCost$meanOfbestOffGroupsMeanExternalCostEachIteration
    worstExternalCost <- outputDataList$externalCost$meanOfworstOffGroupsMeanExternalCostEachIteration
  # Calculate Decision Cost
    decisionCost <- outputDataList$rounds$meanNumberOfProposalsConsideredEachIteration * outputDataList$theInputParameters$perProposalDecisionCost
  # Calculate Total Cost for Mean, Highest and Lowest groups
    meanTotalCost <- meanExternalCost + decisionCost
    bestTotalCost <- bestExternalCost + decisionCost
    worstTotalCost <- worstExternalCost + decisionCost
# Store a few parameters for use in plots
  parameters <- outputDataList$theInputParameters
  numK <- nrow(outputDataList$externalCost)
  K <- seq(1:numK)
# PLOTS
  xRange <- c(0,numK)
  yRange <- c(0,1)  
  ## full yaxis ## yRange <- (0,max(tc.best))
  x_axislabels <- seq(0,numK,by=10) 
  y_axislabels <- seq(0,1,by=.1)
## full yaxis ## y_axislabels <- seq(0,max(tc.best),by=.1)
  plot(xRange, yRange, type="n", main="", xlab="K-Majority Rule", ylab="Expected Costs", ylim=c(0,1), font=2, axes=FALSE, frame=TRUE)
## full yaxis ##  plot(K,ec.worst,type="n", main="", xlab="K-Majority Rule", ylab="Expected Costs", ylim=c(0,max(tc.best)), font=2, axes=FALSE, frame=TRUE)
  axis(side=1, at=x_axislabels)
  axis(side=2, at=y_axislabels)
  lines(K,decisionCost,lty=1,lwd=3,col='black')
  lineNames <- c("decision costs") # For the Legend
  lineLty <- c(1) # For the Legend
  lineLwd <- c(3) # For the Legend
  lineCol <- c("black")
    
  if (plotMeanEC==TRUE){
  lines(K,meanExternalCost,lty=1,lwd=1,col='black')
  lines(K,meanTotalCost,lty=2,lwd=3,col='black')
  lineNames <- c(lineNames, "mean group e.c.", "mean group t.c.")
  lineLty <- c(lineLty,1,2) # For the Legend
  lineLwd <- c(lineLwd,1,3) # For the Legend
  lineCol <- c(lineCol,"black")
  }
  if (plotBestEC==TRUE){
  lines(K,bestExternalCost,lty=1,lwd=1,col='black')
  lines(K,bestTotalCost,lty=2,lwd=3,col='black')
  lineNames <- c(lineNames, "best group e.c.", "best group t.c.")
  lineLty <- c(lineLty,1,2) # For the Legend
  lineLwd <- c(lineLwd,1,3) # For the Legend
  lineCol <- c(lineCol,"black")
  }
  if (plotWorstEC==TRUE){
  lines(K,worstExternalCost,lty=1,lwd=1,col='black')
  lines(K,worstTotalCost,lty=2,lwd=3,col='black')
  lineNames <- c(lineNames, "worst group e.c.", "worst group t.c.")
  lineLty <- c(lineLty,1,2) # For the Legend
  lineLwd <- c(lineLwd,1,3) # For the Legend
  lineCol <- c(lineCol,"black")
  }
  
  mtext(text=paste("Group Size:", paste(outputDataList$theInputParameters$groupSize, collapse=", ")),
        side=3,
        line=1.50,
        adj=0,
        cex=1.1,
        font=2)
  
  mtext(text=paste("Initial Utility:", paste(
                                        toupper(
                                          substring(
                                            outputDataList$allGroups[[1]]$utilityDistribution,1,1)),
                                             "(",
                                                sub('^(-)?0[.]', '\\1.', outputDataList$allGroups[[1]]$utilityDistributionParam1),
                                             ",",
                                                sub('^(-)?0[.]', '\\1.',outputDataList$allGroups[[1]]$utilityDistributionParam2),
                                             ")", 
                                        collapse=", ", sep="")),
        side=3,
        line=1.50,
        adj=1,
        cex=1.1,
        font=2)

  mtext(text=paste("Group Error:", paste(
                                        toupper(
                                          substring(
                                            outputDataList$allGroups[[1]]$errorDistribution,1,1)),
                                            "(",
                                              sub('^(-)?0[.]', '\\1.', outputDataList$allGroups[[1]]$errorDistributionParam1),
                                            ",",
                                              sub('^(-)?0[.]', '\\1.',outputDataList$allGroups[[1]]$errorDistributionParam2),
                                            ")", 
                                          collapse=", ", sep="")),
  side=3,
  line=.25,
  adj=0,
  cex=1.1,
  font=2)

  mtext(text=paste("Change Mean Utility:", paste(outputDataList$theInputParameters$groupPostFailingProposalMeanUiIncrease, collapse=", ")),
        side=3,
        line=.25,
        adj=1,
        cex=1.1,
        font=2)

mtext(text=paste("Per Round Decision Cost:", paste(outputDataList$theInputParameters$perProposalDecisionCost, collapse=", ")),
      side=1,
      line=2.00,
      adj=0,
      cex=1.1,
      font=2)
  
  legend(5,1, # places a legend at the appropriate place
           lineNames, # puts text in the legend 
           lty=lineLty, # gives the legend appropriate symbols (lines)
           lwd=lineLwd, # gives the legend the appropriate weight
           col=lineCol # gives the legend the appropriate color
         ) # 
}  


#------------------------------------------------------------------------------------------------------------
#' plotPareto
#' 
#' Plot the number of rounds (y-axis) where a Pareto preferred proposal failed by k-majority (x-axis)
#' @param outputDataList: The output data list of summaries, that is generated by the iterations() function
#' @return A plot of the umber of rounds (y-axis) where a Pareto preferred proposal failed by k-majority (x-axis).

plotPareto <- function(outputDataList){
  
  par(mar=c(5.1, 4.1, 6.1, 2.1)) # Set top margin?mar
  
  
numberOfIterations <- outputDataList$theInputParameters$numberOfIterations
averageNumParetoFailedPerRound <- outputDataList$proportionParetoFailures 
plot(c(1:length(averageNumParetoFailedPerRound)),averageNumParetoFailedPerRound,type="n",
     main="",
     xlab="K-Majority Rule",
     ylab="Rounds",
     xlim=c(0,length(averageNumParetoFailedPerRound)),
     ylim=c(0,ceiling(max(averageNumParetoFailedPerRound))),
     cex=1.2,font=2)
lines(c(1:length(averageNumParetoFailedPerRound)), averageNumParetoFailedPerRound, col = "Black", lwd = 2, lty = "solid")
# main title
mtext(text="Proportion of Rounds where Pareto Preferred Proposal Failed",side=3,line=3.00,cex=1.0,font=2)


mtext(text=paste("Group Size:", paste(outputDataList$theInputParameters$groupSize, collapse=", ")),
      side=3,
      line=1.50,
      adj=0,
      cex=1.1,
      font=2)

mtext(text=paste("Initial Utility:", paste(
  toupper(
    substring(
      outputDataList$allGroups[[1]]$utilityDistribution,1,1)),
  "(",
  sub('^(-)?0[.]', '\\1.', outputDataList$allGroups[[1]]$utilityDistributionParam1),
  ",",
  sub('^(-)?0[.]', '\\1.',outputDataList$allGroups[[1]]$utilityDistributionParam2),
  ")", 
  collapse=", ", sep="")),
  side=3,
  line=1.50,
  adj=1,
  cex=1.1,
  font=2)

mtext(text=paste("Group Error:", paste(
  toupper(
    substring(
      outputDataList$allGroups[[1]]$errorDistribution,1,1)),
  "(",
  sub('^(-)?0[.]', '\\1.', outputDataList$allGroups[[1]]$errorDistributionParam1),
  ",",
  sub('^(-)?0[.]', '\\1.',outputDataList$allGroups[[1]]$errorDistributionParam2),
  ")", 
  collapse=", ", sep="")),
  side=3,
  line=.25,
  adj=0,
  cex=1.1,
  font=2)

mtext(text=paste("Change Mean Utility:", paste(outputDataList$theInputParameters$groupPostFailingProposalMeanUiIncrease, collapse=", ")),
      side=3,
      line=.25,
      adj=1,
      cex=1.1,
      font=2)

mtext(text=paste("Per Round Decision Cost:", paste(outputDataList$theInputParameters$perProposalDecisionCost, collapse=", ")),
      side=1,
      line=2.00,
      adj=0,
      cex=1.1,
      font=2)
}
  
  #------------------------------------------------------------------------------------------------------------
  #' aKMajorityCostMultiSimulation
  #' Runs a series of proposals for J iterations.
  #' 
  #' @param folderName: A user provided string for the output folder. The name will be output_folderName_on_08_11_2013_at_17_30_25
  #' @param numberOfIterations: The number of iterations the model is run.
  #' @param groupSize: This is a vector that has a length of numberOfGroups. Each element indicates the number of voters in each group.
  #' @param utilityDistribution: The distribution the utility ui values will be drawn from. This can currently only be "normal".
  #' @param utilityDistributionParameters: The relevant parameters needed for the distribution provided as utilityDistribution. You must provide a pair of parameters for each group. For "normal" you must provide the mean and standard deviation.  For example for 5 groups and utilityDistribution="normal", you might provide c(-.7,.2,-.1,.2,0,.2,-.3,.2,.2,.2).
  #' @param errorDistribution: The distribution the error ei values will be drawn from. This can be "uniform", "normal".
  #' @param errorDistributionParameters: The relevant parameters needed for the distribution provided as errorDistribution. For "normal" you must provide the mean and standard deviation. For "uniform" you must provide the lower bound and upper bound. For example for 5 groups and errorDistribution="uniform", you might provide c(-.7,0,-.1,.3,0,.2,-.3,.2,-.2,.2).
  #' @param groupPostFailingProposalMeanUiIncrease: The amount the mean used to sample a group's ui goes up after a failing proposal. Must be entered as a string vector.
  #' @param maximumNumberOfProposalsInASeries: An intiger or FALSE. If an integer, it represents the maximum possible number of proposals considered by a group of voters in one series of votes. Note though, that even if if maximumNumberOfProposalsInASeries is set to an integer, that voting will still stop if a proposal passes for all kMajority Rules being considerd. To allow the series of proposals to anly stop once all kMajority Rules have passes a proposal, set maximumNumberOfProposalsInASeries=FALSE.
  #' @param perProposalDecisionCost: The total decision costs a group faced given the number of rounds (R) it took to pass the proposal. Must be entered as a vector of strings.
  #' @param outputTo: The working directory where you want the output from the simulation to be stored. The default uses your current working directory. Enter the directory as a string appropriate to your operating system, like outputTo="/Users/username/Documents/Foldername/Targetfoldername" or C:/Documents and Settings/username/My Documents/x/y/z"
  #' @param silentSeries: TRUE silences all of the notifications from the running of a series of proposals like "The vote on the proposal in round 126 is complete. kMajorities that passed proposal: 84". FALSE prints all the notifications to the console as the series of proposals runs.
  #' @param silentIterations: TRUE silences the notification that an iteration is complete, like "iteration 9 is complete.". FALSE prints all the notifications to the console as the iterations run.
  #' @param writeCSV: TRUE writes out all of the input and output as CSV files for later use or inspection.
  #' @param writeR: TRUE writes out all the input and output as R objects using dput() for later use or inspection. See ?dput.
  #' @param plotExternalDecisionTotalCosts: TRUE creates two graphs. One has the decision cost, external cost and total cost for the worst groups. The other plots the same three costs, but for the mean groups. This is the main output presented in the paper.
  #' @param plotOnlyExternalCost: TRUE plots the external cost for the worst, mean and best groups.
  #' @param plotNumberOfRounds: TRUE plots mean number of rounds it took for the status quo to be defeated for each k-majority rule, across all the iterations.
  #' @param plotPareto: TRUE plots the proportion of rounds with a failed Pareto Proposal.
  #' @return Stores all model input, output and graphs in provided working directory. The user can control which elemets to store and plot with the function parameters writeCSV, writeRObjects, plotEC, plotDC, plotTC, plotNumberOfRounds and plotPareto. 
  aKMajorityCostMultiSimulation <-  function(folderName,
                                              numberOfIterations,
                                              groupSize,
                                              utilityDistribution,
                                              utilityDistributionParameters,
                                              errorDistribution,
                                              errorDistributionParameters,
                                              groupPostFailingProposalMeanUiIncrease,
                                              maximumNumberOfProposalsInASeries,
                                              perProposalDecisionCost,
                                              outputTo,
                                              silentSeries,
                                              silentIterations,
                                              writeCSV,
                                              writeRObjects,
                                              plotExternalDecisionTotalCosts,
                                              plotOnlyExternalCost,
                                              plotNumberOfRounds,
                                              plotPareto,
                                              printOutputToScreen){

setwd(dir=outputTo)    
cat("**BEGIN SIMULATION**\n")
    
    # STEP 1 Run the iterations
    iterationsOutput <- iterations(numberOfIterations,groupSize,utilityDistribution,utilityDistributionParameters,errorDistribution,errorDistributionParameters,groupPostFailingProposalMeanUiIncrease,maximumNumberOfProposalsInASeries,perProposalDecisionCost,silentSeries,silentIterations)
    
    # STEP 2: Create output directory:
    sysTimeStamp <- format(Sys.time(),"on_%m_%d_%Y_at_%H_%M_%S")
    dir.create(paste("output_",folderName,"_",sysTimeStamp, sep=""), showWarnings=FALSE)
 cat("The simulation output  will be stored in \n  ",outputTo, paste("/output_",folderName,"_",sysTimeStamp,"\n\n", sep=""), sep = "")
   

 # STEP 3: Plots

if(plotNumberOfRounds==TRUE | plotOnlyExternalCost==TRUE | plotExternalDecisionTotalCosts==TRUE | plotPareto==TRUE){
  dir.create(paste("output_",folderName,"_",sysTimeStamp,"/plots",sep=""), showWarnings=FALSE)
  cat("CREATED 'plots' folder.\n\n", sep = "") 
}#ends the if any plots IF


    if (plotNumberOfRounds==TRUE){
    # Plot the mean number of rounds it took for the status quo to be defeated for each k-majority rule, across all the iterations.
    pdf(paste("output_",folderName,"_",sysTimeStamp,"/plots/roundsTillPasses.pdf",sep=""), width = 7, height = 6.12)
    plotNumberOfRounds(iterationsOutput)
    dev.off()
    
    png(paste("output_",folderName,"_",sysTimeStamp,"/plots/roundsTillPasses.png",sep=""), width = 700, height = 612)
    plotNumberOfRounds(iterationsOutput)
    dev.off()
    
      cat("SAVED 'roundsTillPasses.pdf' and 'roundsTillPasses.png' to the 'plots' folder.\n", sep = "")
    } # ends if(plotNumberOfRounds==TRUE)
    
    if (plotOnlyExternalCost==TRUE){
    # Plots only the External Cost measures for worst, best and mean groups.
    pdf(paste("output_",folderName,"_",sysTimeStamp,"/plots/externalCost.pdf",sep=""), width = 7, height = 6.12)
    plotOnlyExternalCost(iterationsOutput)
    dev.off()
    
    png(paste("output_",folderName,"_",sysTimeStamp,"/plots/externalCost.png",sep=""), width = 700, height = 612)
    plotOnlyExternalCost(iterationsOutput)
    dev.off()
    
    cat("SAVED 'externalCost.pdf' and 'externalCost.png' to the 'plots' folder.\n\n", sep = "")
    } # ends if(plotOnlyExternalCost==TRUE)
    
    if(plotExternalDecisionTotalCosts==TRUE){
    # Plots the External Decision and Total Cost measures for mean group.
    pdf(paste("output_",folderName,"_",sysTimeStamp,"/plots/totalCostMeanGroup.pdf",sep=""), width = 7, height = 6.12)
    plotExternalDecisionTotalCost(iterationsOutput,plotMeanEC=TRUE,plotBestEC=FALSE,plotWorstEC=FALSE)
    dev.off()
    
    png(paste("output_",folderName,"_",sysTimeStamp,"/plots/totalCostMeanGroup.png",sep=""), width = 700, height = 612)
    plotExternalDecisionTotalCost(iterationsOutput,plotMeanEC=TRUE,plotBestEC=FALSE,plotWorstEC=FALSE)
    dev.off()
    
    cat("SAVED 'totalCostMeanGroup.pdf' and 'totalCostMeanGroup.png' to the 'plots' folder.\n\n", sep = "")
    
    
    # Plots the External Decision and Total Cost measures for best group.
    pdf(paste("output_",folderName,"_",sysTimeStamp,"/plots/totalCostBestGroup.pdf",sep=""), width = 7, height = 6.12)
    plotExternalDecisionTotalCost(iterationsOutput,plotMeanEC=FALSE,plotBestEC=TRUE,plotWorstEC=FALSE)
    dev.off()
    
    png(paste("output_",folderName,"_",sysTimeStamp,"/plots/totalCostBestGroup.png",sep=""), width = 700, height = 612)
    plotExternalDecisionTotalCost(iterationsOutput,plotMeanEC=FALSE,plotBestEC=TRUE,plotWorstEC=FALSE)
    dev.off()
    
    cat("SAVED 'totalCostBestGroup.pdf' and 'totalCostBestGroup.png' to the 'plots' folder.\n\n", sep = "")
    
    

    # Plots the External Decision and Total Cost measures for worst groups.
    pdf(paste("output_",folderName,"_",sysTimeStamp,"/plots/totalCostWorstGroups.pdf",sep=""), width = 7, height = 6.12)
    plotExternalDecisionTotalCost(iterationsOutput,plotMeanEC=FALSE,plotBestEC=FALSE,plotWorstEC=TRUE)
    dev.off()
    
    png(paste("output_",folderName,"_",sysTimeStamp,"/plots/totalCostWorstGroups.png",sep=""), width = 700, height = 612)
    plotExternalDecisionTotalCost(iterationsOutput,plotMeanEC=FALSE,plotBestEC=FALSE,plotWorstEC=TRUE)
    dev.off()
    
    cat("SAVED 'totalCostWorstGroups.pdf' and 'totalCostWorstGroups.png' to the 'plots' folder.\n\n", sep = "")
                  } #Ends if plotTC==TRUE
    
    if(plotPareto==TRUE){
    # Plots the Proportion of Rounds with a failed Pareto Proposal
    pdf(paste("output_",folderName,"_",sysTimeStamp,"/plots/paretoFailures.pdf",sep=""), width = 7, height = 6.12)
    plotPareto(iterationsOutput)
    dev.off()
    
    png(paste("output_",folderName,"_",sysTimeStamp,"/plots/paretoFailures.png",sep=""), width = 700, height = 612)
    plotPareto(iterationsOutput)
    dev.off()
    
      cat("SAVED 'paretoFailures.pdf' and 'paretoFailures.png' to the 'plots' folder.\n", sep = "")
                      } #ends if(plotPareto==TRUE)

if (writeRObjects==TRUE){
# STEP 4: Store the input parameters and the output as one R object.
dir.create(paste("output_",folderName,"_",sysTimeStamp,"/robjects",sep=""), showWarnings=FALSE)
cat("CREATED 'robjects' folder.\n", sep = "")

          dput(iterationsOutput$theInputParameters, file = paste("output_",folderName,"_",sysTimeStamp,"/robjects/inputAsRobject",sep=""))
          dput(iterationsOutput$rounds, file = paste("output_",folderName,"_",sysTimeStamp,"/robjects/roundsAsRobject",sep=""))
          dput(iterationsOutput$externalCost, file = paste("output_",folderName,"_",sysTimeStamp,"/robjects/externalCostAsRobject",sep=""))
          dput(iterationsOutput$allGroups, file = paste("output_",folderName,"_",sysTimeStamp,"/robjects/allGroupsAsRobject",sep=""))
          dput(iterationsOutput$allVoters, file = paste("output_",folderName,"_",sysTimeStamp,"/robjects/allVotersAsRobject",sep=""))
          dput(iterationsOutput$allYeas, file = paste("output_",folderName,"_",sysTimeStamp,"/robjects/allYeasAsRobject",sep=""))
          dput(iterationsOutput$allPassesForWhich, file = paste("output_",folderName,"_",sysTimeStamp,"/robjects/allPassesForWhichRobject",sep=""))
          dput(iterationsOutput$allMeanVoters, file = paste("output_",folderName,"_",sysTimeStamp,"/robjects/allMeanVotersRobject",sep=""))
          dput(iterationsOutput$allHighestMeanGroup, file = paste("output_",folderName,"_",sysTimeStamp,"/robjects/allHighestMeanGroupRobject",sep=""))
          dput(iterationsOutput$allLowestMeanGroup, file = paste("output_",folderName,"_",sysTimeStamp,"/robjects/allLowestMeanGroupRobject",sep=""))
        cat("SAVED input and output as one R Object in the 'robjects' folder. \n  It can be read into R with dget(). \n  See ?dget for more information. \n", sep = "")
} #ends if(writeRObjects==TRUE)
            
    # STEP 5: Store the objects as .csv files.
    if (writeCSV==TRUE){
dir.create(paste("output_",folderName,"_",sysTimeStamp,"/csvs",sep=""), showWarnings=FALSE)
cat("CREATED 'csvs' folder.\n", sep = "")      
      
            # write the input Parameters as .csv files    
            write.csv(as.matrix(iterationsOutput$theInputParameters), file = paste("output_",folderName,"_",sysTimeStamp,"/csvs/inputParameters.csv",sep=""),row.names=TRUE)
              cat("SAVED 'inputParameters.csv' in the 'csv' folder. \n  It can be read into R with read.csv. \n  See ?read.csv. \n", sep = "")

      # write the rounds as .csv files   
          write.csv(as.matrix(iterationsOutput$rounds), file = paste("output_",folderName,"_",sysTimeStamp,"/csvs/rounds.csv",sep=""),row.names=TRUE)
          cat("SAVED 'rounds.csv' in the 'csv' folder. \n  It can be read into R with read.csv. \n  See ?read.csv. \n", sep = "")
            
      # write the voters as .csv files
            for (k in 1:length(iterationsOutput$allVoters)){
            write.csv(iterationsOutput$allVoters[[k]], file = paste("output_",folderName,"_",sysTimeStamp,"/csvs/votersIteration_",k,".csv", sep=""),row.names=FALSE)
            }
              cat("SAVED each 'votersIteration_k.csv' files in the 'csv' folder. \n  They can be read into R with read.csv. \n  See ?read.csv. \n", sep = "")

      # write the groups as .csv files
          for (k in 1:length(iterationsOutput$allGroups)){
            write.csv(iterationsOutput$allGroups[[k]], file = paste("output_",folderName,"_",sysTimeStamp,"/csvs/groupsIteration_",k,".csv", sep=""),row.names=FALSE)
          }
          cat("SAVED each 'groupIteration_k.csv' files in the 'csv' folder. \n  They can be read into R with read.csv. \n  See ?read.csv. \n", sep = "")

      # write the yeas total for each iteration as .csv
          write.csv(as.matrix(iterationsOutput$allYeas), file = paste("output_",folderName,"_",sysTimeStamp,"/csvs/allYeas.csv",sep=""),row.names=FALSE)
              cat("SAVED 'allYeas.csv', which contains the total number of yea voters for each kMajority rule in each iteration in the 'csv' folder. \n  It can be read into R with read.csv. \n  See ?read.csv. \n", sep = "")
          
      # write the passes for which kMajority for each iteration as .csv
        for (k in 1:length(iterationsOutput$allPassesForWhich)){
      write.csv(iterationsOutput$allPassesForWhich[[k]], file = paste("output_",folderName,"_",sysTimeStamp,"/csvs/passesForWhich_",k,".csv", sep=""),row.names=FALSE)
        }
        cat("SAVED each 'passesForWhich_k.csv' files in the 'csv' folder. \n  They can be read into R with read.csv. \n  See ?read.csv. \n", sep = "")

  # Write the External Cost Loss Measures as .csv files
      write.csv(iterationsOutput$externalCost$meanOfMeanVotersExternalCostEachIteration, file = paste("output_",folderName,"_",sysTimeStamp,"/csvs/meanOfMeanVotersExternalCostEachIteration.csv",sep=""),row.names=FALSE)
cat("SAVED 'meanOfMeanVotersExternalCostEachIteration.csv', which records the utility of the min voter for a given kMajority in each iteration. \n  It has been saved in the 'csv' folder. It can be read into R with read.csv. \n  See ?read.csv. \n\n", sep = "")

      write.csv(iterationsOutput$externalCost$meanOfbestOffGroupsMeanExternalCostEachIteration, file = paste("output_",folderName,"_",sysTimeStamp,"/csvs/meanOfbestOffGroupsMeanExternalCostEachIteration.csv",sep=""),row.names=FALSE)
cat("SAVED 'meanOfbestOffGroupsMeanExternalCostEachIteration.csv', which records the utility of the min voter for a given kMajority in each iteration. \n  It has been saved in the 'csv' folder. It can be read into R with read.csv. \n  See ?read.csv. \n\n", sep = "")

    write.csv(iterationsOutput$externalCost$meanOfworstOffGroupsMeanExternalCostEachIteration, file = paste("output_",folderName,"_",sysTimeStamp,"/csvs/meanOfworstOffGroupsMeanExternalCostEachIteration.csv",sep=""),row.names=FALSE)
cat("SAVED 'meanOfworstOffGroupsMeanExternalCostEachIteration.csv', which records the utility of the min voter for a given kMajority in each iteration. \n  It has been saved in the 'csv' folder. It can be read into R with read.csv. \n  See ?read.csv. \n\n", sep = "")
                    } # ends if(writeCSV==TRUE)
                    
sink(paste("output_",folderName,"_",sysTimeStamp,"/",nrow(iterationsOutput$allGroups[[1]]),"G_",iterationsOutput$theInputParameters$utilityDistribution[1],"Util_",iterationsOutput$theInputParameters$errorDistribution[1],"Error_Info.txt",sep=""))
          cat("This file is: ",nrow(iterationsOutput$allGroups[[1]]),"G_",iterationsOutput$theInputParameters$utilityDistribution[1],"Util_",iterationsOutput$theInputParameters$errorDistribution[1],"Error_Info.txt\n\n",sep="")
          cat("This file contains basic information about the simulation files contained in this folder.\n\n")
          cat("Based on",numberOfIterations," Iterations\n\n")
          cat("Rounds Limit =",iterationsOutput$theInputParameters$maximumNumberOfProposalsInASeries,"\n\n")
          cat(nrow(iterationsOutput$allGroups[[1]]), "Groups: ", iterationsOutput$allGroups[[1]]$groupSize,"\n\n")
          cat("Utility Drawn From: \n")
           for (i in 1:nrow(iterationsOutput$allGroups[[1]])){
             cat("Group ",i,": ",as.vector(iterationsOutput$allGroups[[1]]$utilityDistribution[i]),"(",iterationsOutput$allGroups[[1]]$utilityDistributionParam1[i],",",iterationsOutput$allGroups[[1]]$utilityDistributionParam2[i],")\n",sep="")
           }
          cat("\nError Drawn From: \n")
          for (i in 1:nrow(iterationsOutput$allGroups[[1]])){
            cat("Group ",i,": ",as.vector(iterationsOutput$allGroups[[1]]$errorDistribution[i]),"(",iterationsOutput$allGroups[[1]]$errorDistributionParam1[i],",",iterationsOutput$allGroups[[1]]$errorDistributionParam2[i],")\n",sep="")
          }
          cat("\nGroup Alpha: \n")
          for (i in 1:nrow(iterationsOutput$allGroups[[1]])){
            cat("Group ",i,": ", as.vector(iterationsOutput$allGroups[[1]]$groupPostFailingProposalMeanUiIncrease[i]),"\n",sep="")
          }


## The following output is only needed for multiple round cases
if (iterationsOutput$theInputParameters$maximumNumberOfProposalsInASeries > 1){
          cat("Per-Round Decision Cost: ",iterationsOutput$theInputParameters$perProposalDecisionCost,"\n\n")
          cat("Min Rounds of Voting by kMaj: ",iterationsOutput$rounds$minRoundProposalPassedEachIteration,"\n\n")
          cat("Mean Rounds of Voting by kMaj: ",iterationsOutput$rounds$meanRoundProposalPassedEachIteration,"\n\n")
          cat("Max Rounds of Voting by kMaj: ",iterationsOutput$rounds$maxRoundProposalPassedEachIteration,"\n\n")
          cat("Mean Proposals Considered by kMaj: ",iterationsOutput$rounds$meanNumberOfProposalsConsideredEachIteration,"\n\n")
          cat("Decision Costs by kMaj: ",iterationsOutput$rounds$meanNumberOfProposalsConsideredEachIteration * iterationsOutput$theInputParameters$perProposalDecisionCost,"\n\n")
} else {cat("Note there are no Decision Costs for this run because there is only one round of voting.\n\n")}

          cat("Mean External Cost by kMaj: ",iterationsOutput$externalCost$meanOfMeanVotersExternalCostEachIteration,"\n\n")
          cat("Best External Cost by kMaj: ",iterationsOutput$externalCost$meanOfbestOffGroupsMeanExternalCostEachIteration,"\n\n")
          cat("Worst External Cost by kMaj: ",iterationsOutput$externalCost$meanOfworstOffGroupsMeanExternalCostEachIteration,"\n\n")

cat("Mean Total Cost by kMaj: ",iterationsOutput$externalCost$meanOfMeanVotersExternalCostEachIteration+(iterationsOutput$rounds$meanNumberOfProposalsConsideredEachIteration * iterationsOutput$theInputParameters$perProposalDecisionCost),"\n\n")
cat("Best Total Cost by kMaj: ",iterationsOutput$externalCost$meanOfbestOffGroupsMeanExternalCostEachIteration+(iterationsOutput$rounds$meanNumberOfProposalsConsideredEachIteration * iterationsOutput$theInputParameters$perProposalDecisionCost),"\n\n")
cat("Worst Total Cost by kMaj: ",iterationsOutput$externalCost$meanOfworstOffGroupsMeanExternalCostEachIteration+(iterationsOutput$rounds$meanNumberOfProposalsConsideredEachIteration * iterationsOutput$theInputParameters$perProposalDecisionCost),"\n\n")
sink()

cat("The simulation output was be stored in \n  ",outputTo, paste("/output_",folderName,"_",sysTimeStamp,"\n\n**END SIMULATION**\n", sep=""), sep = "")




### This prints the output to the screen if printToScreen==TRUE
if (printOutputToScreen==TRUE){
  cat("\n\n This output contains basic information about the simulation that was just run.\n\n")
  cat("Based on",numberOfIterations," Iterations\n\n")
  cat("Rounds Limit =",iterationsOutput$theInputParameters$maximumNumberOfProposalsInASeries,"\n\n")
  cat(nrow(iterationsOutput$allGroups[[1]]), "Groups: ", iterationsOutput$allGroups[[1]]$groupSize,"\n\n")
  cat("Utility Drawn From: \n")
  for (i in 1:nrow(iterationsOutput$allGroups[[1]])){
    cat("Group ",i,": ",as.vector(iterationsOutput$allGroups[[1]]$utilityDistribution[i]),"(",iterationsOutput$allGroups[[1]]$utilityDistributionParam1[i],",",iterationsOutput$allGroups[[1]]$utilityDistributionParam2[i],")\n",sep="")
  }
  cat("\nError Drawn From: \n")
  for (i in 1:nrow(iterationsOutput$allGroups[[1]])){
    cat("Group ",i,": ",as.vector(iterationsOutput$allGroups[[1]]$errorDistribution[i]),"(",iterationsOutput$allGroups[[1]]$errorDistributionParam1[i],",",iterationsOutput$allGroups[[1]]$errorDistributionParam2[i],")\n",sep="")
  }
  cat("\nGroup Alpha: \n")
  for (i in 1:nrow(iterationsOutput$allGroups[[1]])){
    cat("Group ",i,": ", as.vector(iterationsOutput$allGroups[[1]]$groupPostFailingProposalMeanUiIncrease[i]),"\n",sep="")
  }
  
  
  ## The following output is only needed for multiple round cases
  if (iterationsOutput$theInputParameters$maximumNumberOfProposalsInASeries > 1){
    cat("Per-Round Decision Cost: ",iterationsOutput$theInputParameters$perProposalDecisionCost,"\n\n")
    cat("Min Rounds of Voting by kMaj: ",iterationsOutput$rounds$minRoundProposalPassedEachIteration,"\n\n")
    cat("Mean Rounds of Voting by kMaj: ",iterationsOutput$rounds$meanRoundProposalPassedEachIteration,"\n\n")
    cat("Max Rounds of Voting by kMaj: ",iterationsOutput$rounds$maxRoundProposalPassedEachIteration,"\n\n")
    cat("Mean Proposals Considered by kMaj: ",iterationsOutput$rounds$meanNumberOfProposalsConsideredEachIteration,"\n\n")
    cat("Decision Costs by kMaj: ",iterationsOutput$rounds$meanNumberOfProposalsConsideredEachIteration * iterationsOutput$theInputParameters$perProposalDecisionCost,"\n\n")
  } else {cat("Note there are no Decision Costs for this run because there is only one round of voting.\n\n")}
  
  cat("Mean External Cost by kMaj: ",iterationsOutput$externalCost$meanOfMeanVotersExternalCostEachIteration,"\n\n")
  cat("Best External Cost by kMaj: ",iterationsOutput$externalCost$meanOfbestOffGroupsMeanExternalCostEachIteration,"\n\n")
  cat("Worst External Cost by kMaj: ",iterationsOutput$externalCost$meanOfworstOffGroupsMeanExternalCostEachIteration,"\n\n")
  
  cat("Mean Total Cost by kMaj: ",iterationsOutput$externalCost$meanOfMeanVotersExternalCostEachIteration+(iterationsOutput$rounds$meanNumberOfProposalsConsideredEachIteration * iterationsOutput$theInputParameters$perProposalDecisionCost),"\n\n")
  cat("Best Total Cost by kMaj: ",iterationsOutput$externalCost$meanOfbestOffGroupsMeanExternalCostEachIteration+(iterationsOutput$rounds$meanNumberOfProposalsConsideredEachIteration * iterationsOutput$theInputParameters$perProposalDecisionCost),"\n\n")
  cat("Worst Total Cost by kMaj: ",iterationsOutput$externalCost$meanOfworstOffGroupsMeanExternalCostEachIteration+(iterationsOutput$rounds$meanNumberOfProposalsConsideredEachIteration * iterationsOutput$theInputParameters$perProposalDecisionCost),"\n\n")
}

iterationsOutput
} # ends the aKMajorityCostMultiSimulation() function.


#Figure 4B
FourB <- aKMajorityCostMultiSimulation(folderName="Figure4B", 
                                       numberOfIterations=1000, 
                                       groupSize=c(20,30,50),
                                       utilityDistribution=c("normal","normal","normal"), 
                                       utilityDistributionParameters=c(-.3,.2,0,.2,.3,.2), 
                                       errorDistribution=c("normal","normal","normal"), 
                                       errorDistributionParameters=c(0,.3,0,.3,0,.3), 
                                       groupPostFailingProposalMeanUiIncrease=c("0","0","0"),
                                       maximumNumberOfProposalsInASeries=1, 
                                       perProposalDecisionCost=0, 
                                       outputTo=getwd(), 
                                       silentSeries=TRUE, 
                                       silentIterations=FALSE,
                                       writeCSV=FALSE,
                                       writeRObjects=FALSE,
                                       plotExternalDecisionTotalCosts=FALSE,
                                       plotOnlyExternalCost=TRUE,
                                       plotNumberOfRounds=FALSE,
                                       plotPareto=FALSE,
                                       printOutputToScreen=TRUE)
