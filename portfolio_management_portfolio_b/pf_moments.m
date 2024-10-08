function [r,s2] = pf_moments(w,R,SIGMA)
r = w'*R;  %mean
s2 = w'*SIGMA*w; %covariance
end