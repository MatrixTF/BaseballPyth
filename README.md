# BaseballPyth
An update on MLB's Pythagorean Formula for win percentage and subsequent comparison to a simple linear regression using run differential.

*****************************************************************************************
Author: Andy Alexander <br />
Date Created: 10/29/2016

Purpose:   
1. Determine the optimal exponent for the Pythagorean Theorem of Baseball <br />
2. Compare updated Pythagorean formula to a simple linear regression of run differential
           
Original Pytahagorean Formula (created by Bill James): <br />
Win% = (Runs Scored)^2/[(Runs Scored)^2 + (Runs Allowed)^2]

Data: Lahman files for all Major League Baseball games from 1871-2015 <br />
https://www.kaggle.com/seanlahman/the-history-of-baseball

Goal: Determine x by least squares analysis for: ((RS)^x/[(RS)^x + (RA)^x])*Games - Wins

***************************************************************************************** 

