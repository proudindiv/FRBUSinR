% generate_freddata.m
% =========================================================================
% DESCRIPTION: 
% This script loads in raw data from a monthly database CSV file,
% transforms each series based on transformation code using
% prepare_missing.m, and removes outliers from the transformed data using
% remove_outliers.m.
%
% NOTE:
% The default CSV file read by this code is 2015-04.csv, which is the April
% 2015 version of the dataset. If using a different version, make sure to
% change the variable "csv_in" on line 26 to match the name of the relevant
% CSV file.
%
% =========================================================================
% CLEAR: 
clear all
close all
clc

% =========================================================================
% PARAMETER TO BE CHANGED:
% Update the .csv filename to match the desired version

% CSV file name
csv_in='2015-04.csv';

% =========================================================================
% LOAD AND LABEL DATA: 
% Load data from CSV file
dum=importdata(csv_in,',');

% Variable names
names=dum.textdata(1,2:end);

% Transformation numbers
tcode=dum.data(1,:);

% Raw data
rawdata=dum.data(2:end,:);

% Month of final observation
final_month=month(dum.textdata(end,1));

% Year of final observation
final_year=year(dum.textdata(end,1));

% =========================================================================
% SET UP DATES: 
% Dates (monthly) are of the form YEAR+MONTH/12
% e.g. March 1970 is represented as 1970+3/12
% Dates go from 1959:01 to final_year:final_month (see above)
dates = [1959+1/12:1/12:final_year+final_month/12]';

% T = number of months in sample
T=size(dates,1);
rawdata=rawdata(1:T,:);

% =========================================================================
% TRANSFORM RAW DATA INTO STATIONARY FORM: 
% Use function prepare_missing.m
%   Output yt: matrix containing data after transformation
yt=prepare_missing(rawdata,tcode);

% =========================================================================
% REDUCE SAMPLE TO USABLE DATES: 
% Remove first two months because some series have been second differenced
yt=yt(3:T,:);
dates=dates(3:T,:);

% =========================================================================
% REMOVE OUTLIERS: 
% Use function remove_outliers.m (see for definition of outliers)
%   Output data: matrix containing transformed series after removal of outliers
%   Output n: matrix containing number of outliers removed for each series
[data,n]=remove_outliers(yt);

% =========================================================================
% SAVE DATA TO .MAT FILE: 
% Save data, dates, names, and tcode to file freddata.mat
save freddata data dates names tcode ;