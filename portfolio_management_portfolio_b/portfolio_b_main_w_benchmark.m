

%% PORTFOLIO MANAGEMENT, PORTFOLIO B W BENCHMARKING

%%% prepare data
start_date = "2020-09-23";
end_date = "2024-10-04";


%% FF5FM data (dowloaded from https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html at 'Fama/French 5 Factors (2x3) [Daily]')

ff5 = readtable('./F-F_Research_Data_5_Factors_2x3_daily.CSV');

%convert to datetime from string format 'yyyyMMdd' and then change the actual datetime
%format to 'yyyy-MM-dd'.
ff5.('Date') = datetime(num2str(ff5.Var1),'InputFormat','yyyyMMdd','Format','yyyy-MM-dd'); %convert to datatime format
ff5.Var1 = []; %get rid of the original series
ff5 = ff5(ff5.Date >= start_date & ff5.Date <= end_date, :); % include only data from start_date to end_date

ff5 = table2timetable(ff5,'RowTimes','Date');

%% asset price data (downloaded with R's quantmod)

data = readtable('./russell_2000.csv');

asset_price_names = data.Properties.VariableNames(contains(data.Properties.VariableNames,"_Adjusted"));
stock_names = strrep(asset_price_names,'_Adjusted','');


%add simple returns
for i = 1:length(asset_price_names)
   data.( [stock_names{i} '_SimpleRet'] ) =...
       ret(data{:,asset_price_names{i}},'simple',true); %returns
end

asset_price_names = data.Properties.VariableNames(2:6);
stock_names = strrep(asset_price_names,'_Adjusted','');

%get rid of first row NaN
data = data(2:end,:);

data = table2timetable(data,'RowTimes','Date');

%% join the tables on the key
%data = join(data,ff5);
data2 = outerjoin(data,ff5); %join with uneven times
data2 = data2(~any(ismissing(data2),2),:); %remove missing rows

summary(data2)


%% SPLIT TO TRAIN AND TEST DATA

train_start_date = "2020-09-23";
train_end_date = "2023-10-04";

test_start_data = "2023-10-05";
test_end_data = "2024-10-04";


data_train = data2(data2.Date >= train_start_date & data2.Date <= train_end_date,:);
data_test = data2(data2.Date >= test_start_data & data2.Date <= test_end_data,:);


%% estimate the slopes for FF5FM
asset_ret_names = data_train.Properties.VariableNames(9:13);
N = length(asset_ret_names);
T = size(data_train,1);

beta = zeros(6, N);
s2 = zeros(N,1);
aic = zeros(N,1);
bic = zeros(N,1);
ll = zeros(N,1);
factors = [data_train.Mkt_RF, data_train.RMW, data_train.CMA, data_train.SMB, data_train.HML];

for i = 1:N
   %excess returns
   y = data_train{:,asset_ret_names{i}} - data_train.RF; %excess returns
   X = [ones(T,1), factors];
   [beta(:,i),s2(i),aic(i),bic(i),ll(i),u] = wls(X,y,[]);
   
   %store the excess returns to the data
   data_train.([asset_ret_names{i} '_RFExcess']) = y;
   data_train.([asset_ret_names{i} '_FF5FMExcess']) = X*beta(:,i); %should one add RF back t this??
end


%% optimize weights (corrected AFTER presentation!)
samp_names = data_train.Properties.VariableNames(contains(data_train.Properties.VariableNames,"_RFExcess"));
Rsamp = mean(data_train{:,samp_names },1)';
%Rff5fm = mean(data_train{:,samp_names },1)' + (beta')*(mean(X,1))';
%Rff5fm = mean(X*beta,1)';
Rff5fm = mean(data_train{:,strrep(asset_ret_names,'_RFExcess','')},1)' + beta(2:end,:)'*mean(factors,1)';

Csamp = cov(data_train{:,samp_names },0);
%Cff5fm = (beta'*cov(X))*beta + diag(cov(data_train{:,samp_names },0)); %VCV, diagonal sample covariance
%Cff5fm = (beta'*cov(X))*beta + cov(data_train{:,samp_names },0); %VCV, full sample covariance
Cff5fm = (beta(2:end,:)'*cov(factors))*beta(2:end,:) + diag(diag(cov(data_train{:,samp_names },0))); %VCV, diagonal sample covariance
%Cff5fm = (beta(2:end,:)'*cov(factors))*beta(2:end,:) + cov(data_train{:,samp_names },0); %VCV, full sample covariance

disp([Rsamp, Rff5fm]);
disp(Csamp);
disp(Cff5fm);


%% plot the portfolios

% simulate efficient fronts
no_samp = 500000;
print_int = 10000;
min_pf_ret = 0;
max_pf_var = inf;
w_lo = -ones(N,1);
w_hi = ones(N,1);

gamma = [1,2,4]; %risk tolerance
rf = 0; %risk free (already accounted for)

pf_moments_samp = @(w)pf_moments(w, Rsamp, Csamp);
pf_moments_ff5fm = @(w)pf_moments(w, Rff5fm, Cff5fm);

%

%% optimize for the sample moments
%[pf_vars, pf_rets, w_try_range, opt] = sim_eff_front_v2(pf_moments_fun, w_lo, w_hi, min_pf_ret, max_pf_var, no_sim, gamma, rf, print_int)
[pf_vars_samp, pf_rets_samp, w_try_range_samp, opt_samp] = sim_eff_front_v2(pf_moments_samp, w_lo, w_hi, min_pf_ret, max_pf_var, no_samp, gamma, rf, print_int);
disp(w_try_range_samp);

%%
f_samp = figure;
hold on;
pp = plot(pf_vars_samp, pf_rets_samp, 'xk', 'MarkerSize',1, 'DisplayName', 'pfs'); pp.Color = [0,0,0,0.1];
plot(pf_vars_samp(opt_samp.max_sr_idx), pf_rets_samp(opt_samp.max_sr_idx), 'or', 'MarkerSize', 10, 'DisplayName', 'max sharpe');
plot(pf_vars_samp(opt_samp.max_r_idx), pf_rets_samp(opt_samp.max_r_idx), 'ob', 'MarkerSize', 10, 'DisplayName', 'max return');
plot(pf_vars_samp(opt_samp.min_s2_idx), pf_rets_samp(opt_samp.min_s2_idx), 'og', 'MarkerSize', 10, 'DisplayName', 'min var');
hold off;
xlabel('pf var');
ylabel('pf ret');
legend show;
%title('sample moments');

disp('equal weight, sample');
[r_ew_sample, s2_ew_sample] = pf_moments_samp(ones(N,1)/N);
disp(r_ew_sample);
disp(s2_ew_sample);
disp(r_ew_sample/sqrt(s2_ew_sample));

disp('maximum sharpe ratio, sample');
disp(opt_samp.max_sr);
disp(opt_samp.max_sr_w');

disp('maximum return, sample');
disp(opt_samp.max_r);
disp(opt_samp.max_r_w');

disp('minimum variance, sample');
disp(opt_samp.min_s2);
disp(opt_samp.min_s2_w');

%% optimize for the ff5fm

%[pf_vars, pf_rets, w_try_range, opt] = sim_eff_front_v2(pf_moments_fun, w_lo, w_hi, min_pf_ret, max_pf_var, no_sim, gamma, rf, print_int)
[pf_vars_ff5fm, pf_rets_ff5fm, w_try_range_ff5fm, opt_ff5fm] = sim_eff_front_v2(pf_moments_ff5fm, w_lo, w_hi, min_pf_ret, max_pf_var, no_samp, gamma, rf, print_int);
disp(w_try_range_ff5fm);

%%
f_ff5fm = figure;
hold on;
pp = plot(pf_vars_ff5fm, pf_rets_ff5fm, 'xk', 'MarkerSize',1, 'DisplayName', 'pfs'); pp.Color = [0,0,0,0.1];
plot(pf_vars_ff5fm(opt_ff5fm.max_sr_idx), pf_rets_ff5fm(opt_ff5fm.max_sr_idx), 'or', 'MarkerSize', 10, 'DisplayName', 'max sharpe');
plot(pf_vars_ff5fm(opt_ff5fm.max_r_idx), pf_rets_ff5fm(opt_ff5fm.max_r_idx), 'ob', 'MarkerSize', 10, 'DisplayName', 'max return');
plot(pf_vars_ff5fm(opt_ff5fm.min_s2_idx), pf_rets_ff5fm(opt_ff5fm.min_s2_idx), 'og', 'MarkerSize', 10, 'DisplayName', 'min var');
hold off;
xlabel('pf var');
ylabel('pf ret');
legend show;
%title('ff5fm moments');

disp('equal weight, ff5fm');
[r_ew_ff5fm, s2_ew_ff5fm] = pf_moments_ff5fm(ones(N,1)/N);
disp(r_ew_ff5fm);
disp(s2_ew_ff5fm);
disp(r_ew_ff5fm/sqrt(s2_ew_ff5fm));

disp('maximum sharpe ratio, ff5fm');
disp(opt_ff5fm.max_sr);
disp(opt_ff5fm.max_sr_w');

disp('maximum return, ff5fm');
disp(opt_ff5fm.max_r);
disp(opt_ff5fm.max_r_w');

disp('minimum variance, ff5fm');
disp(opt_ff5fm.min_s2);
disp(opt_ff5fm.min_s2_w');

%% Plots

% all asset prices
figure; 
hold on;
for i = 1:N
   plot(data_train{:,asset_price_names{i}}, 'Display',asset_price_names{i});
end
hold off;
legend show;



% cumulative returns
all_assets = data_train.Properties.VariableNames(9:15);
all_assets_labels = strrep(all_assets,'_SimpleRet','');


figure; 
hold on;
for i = 1:length(all_assets)
    r_ = data_train{:,all_assets{i}};
    cr_ = 100*(cumprod(1 + r_/100) - 1); %with: cumprod(1 + r) - 1
    plot(cr_, 'DisplayName',all_assets_labels{i});
end
hold off;
legend show;


%% Descriptives
dec = 3;
disp('Train data:');
fprintf('%15s | %8s %8s %8s %8s %8s %8s\n', 'NAME', 'MIN', 'MAX', 'MEAN','STD','SKEW','KURT');
for i = 1:length(all_assets)
    tmp = data_train{:,all_assets{i}};
    fprintf('%15s | %8.3f %8.3f %8.3f %8.3f %8.3f %8.3f\n',...
    all_assets{i},...
    min(tmp),...
    max(tmp),...
    mean(tmp),...
    std(tmp),...
    skewness(tmp),...
    kurtosis(tmp)...
    );
end
 

%asset correlations
corr(data_train{:,all_assets(1:5)})

%asset and factor correlations
disp([all_assets(1:5),'Mkt_RF','SMB','HML','RMW','CMA'])
corr(data_train{:,[all_assets{1:5},{'Mkt_RF','SMB','HML','RMW','CMA'}]})

%

disp('');
disp('Test data:');
fprintf('%15s | %8s %8s %8s %8s %8s %8s\n', 'NAME', 'MIN', 'MAX', 'MEAN','STD','SKEW','KURT');
for i = 1:length(all_assets)
    tmp = data_test{:,all_assets{i}};
    fprintf('%15s | %8.3f %8.3f %8.3f %8.3f %8.3f %8.3f\n',...
    all_assets{i},...
    min(tmp),...
    max(tmp),...
    mean(tmp),...
    std(tmp),...
    skewness(tmp),...
    kurtosis(tmp)...
    );
end
 

%asset correlations
corr(data_test{:,all_assets(1:5)})

%asset and factor correlations
disp([all_assets(1:5),{'Mkt_RF','SMB','HML','RMW','CMA'}])
corr(data_test{:,[all_assets{1:5},{'Mkt_RF','SMB','HML','RMW','CMA'}]})


%% BENCHMARK 

rebalance_interval = '';



