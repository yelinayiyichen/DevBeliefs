
% fmincon model fitting
% Modified by Gail Rosenbaum and Yelina Chen
% To run this script, substitute the file name on line 16
% with the condition data and press "Run" 


clearvars

rng shuffle

dataDir = '/'; 
outputDir='/'; 
fileName=['yourfile_' date '.csv'];


a={[dataDir 'data.csv']};

all_subjects=[]; 

data = readtable(a{1});

subs = unique(data.SID);
nSubs=size(subs,1);

Fit.LB = [0 1e-6];
Fit.UB = [3.5 10];
Fit.Nparms = size(Fit.UB,2); %alpha,beta

for s = 1:length(subs)
    
    sIDchar=subs(s);  
    subdata = data(find(strcmp(data.SID, sIDchar)),:);
    
    if mod(s,100) == 0
        fprintf('Fitting subject %d out of %d...\n',s,nSubs);
    end
    niter = 10;
    for iter = 1:niter  
        fprintf('Iteration %d...\n',iter);
        
        Fit.init(s,iter,:) = rand(1,length(Fit.LB)).*(Fit.UB-Fit.LB)+Fit.LB; % random parameter initialization
        
        [res,lik] = ...
            fmincon(@(x) Yelina_fmincon_model_realData_p2(subdata,x(1),x(2)),...
            squeeze(Fit.init(s,iter,:)),[],[],[],[],Fit.LB,Fit.UB,[],...
            optimset('maxfunevals',10000,'maxiter',2000,'GradObj','off','DerivativeCheck','off','LargeScale','on','Algorithm','active-set'));
        % GradObj = 'on' to use gradients, 'off' to not use them *** ask us about this if you are interested ***
        % DerivativeCheck = 'on' to have fminsearch compute derivatives numerically and check the ones I supply
        % LargeScale = 'on' to use large scale methods, 'off' to use medium
        Fit.Result.Alpha(s,iter) = res(1);
        Fit.Result.Beta(s,iter) = res(2);
        Fit.Result.Lik(s,iter) = lik;
        Fit.Result.Lik  % to view progress so far
    end
    [a, b] = min(Fit.Result.Lik(s, :));
    Fit.Result.BestFit(s,:) = [s,...
        Fit.Result.Alpha(s,b),...
        Fit.Result.Beta(s,b),...
        Fit.Result.Lik(s,b)];
    
end

fitmat = Fit.Result.BestFit

fittable = array2table(fitmat,'VariableNames',{'Idx','Alpha','Beta','LogLik'});

subtable = array2table(subs); 

fulltable = [subtable, fittable];

if ~exist(outputDir, 'dir')
    mkdir(outputDir)
end

writetable(fulltable,[outputDir fileName]);
