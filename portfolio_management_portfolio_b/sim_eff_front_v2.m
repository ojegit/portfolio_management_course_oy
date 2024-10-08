function [pf_vars, pf_rets, w_try_range, opt] = sim_eff_front_v2(pf_moments_fun, w_lo, w_hi, min_pf_ret, max_pf_var, no_sim, gamma, rf, print_int)

%{
Simulates the effcient frontier given expected returns and
variance-covariance matrix of assets

TBA: pr returns ranges and pf variance ranges
%}

[m1,n1] = size(w_lo);
[m2,n2] = size(w_hi);
if m1~=m2 || n1~=n2
    error('w_lo and w_hi must be column vectors with equal dimensions');
end

lo = w_lo;
hi = w_hi;

pf_vars = zeros(no_sim,1);
pf_rets = zeros(no_sim,1);

w_try_range = zeros(m1,2);
max_sr = -Inf;
max_r = -Inf;
min_s2 = Inf;
ng = length(gamma);
max_util = -Inf(ng,1);
opt = struct();

i = 0;
while (1==1)
    
   %draw random numbers from uniform dist and normalize
   v = lo + (hi - lo) .* rand(m1,1);
   w_try = v/sum(v,1);
   
   %discard out of bounds weights
   if any(w_try < w_lo) || any(w_try > w_hi)
       continue
   end
   
    %evaluate pf ret, var and sr
    [r,s2] =  pf_moments_fun(w_try);
    %assert(numel(r)==1);
    %assert(numel(s2)==1);
   
   
    %discard out of bounds returns and variance 
    if any(r < min_pf_ret) || any(s2 > max_pf_var)
        continue
    end
   
    i = i + 1;
    
    %keep track of w's range
    w_try_range(:,1) = min([w_try, w_try_range(:,1)],[],2);
    w_try_range(:,2) = max([w_try, w_try_range(:,2)],[],2);
    
    if print_int > 0 && mod(i,print_int) == 0
       fprintf("SIM NO %d/%d (%03.2f %%)\n",i,no_sim,100*i/no_sim);
    end
   
    pf_vars(i,1) = s2;
    pf_rets(i,1) = r;
    
    
    %%% track optima 
    %maximum sharpe ratio
    sr = (r-rf)/sqrt(s2);
    if sr > max_sr
        max_sr = sr;
        opt.max_sr = sr;
        opt.max_sr_w = w_try;
        opt.max_sr_idx = i;
    end
    
    %maximum return
    if r > max_r
        max_r = r;
        opt.max_r = r;
        opt.max_r_w = w_try;
        opt.max_r_idx = i;
    end
    
    %minimum variance
    if s2 < min_s2
        min_s2 = s2;
        opt.min_s2 = s2;
        opt.min_s2_w = w_try;
        opt.min_s2_idx = i;
    end
    
    %maximum utility (quadratic)
    for j = 1:ng
        util = r - 0.5*gamma(j)*s2;
        if util > max_util(j)
           max_util(j) = util;
           opt.max_util(j) = util;
           opt.max_util_w(:,j) = w_try;
           opt.max_util(j) = i;
        end
    end
    %%% 
    
    
    if i == no_sim
        if print_int > 0
            disp('Done!');
        end
        break;
    end
    
end