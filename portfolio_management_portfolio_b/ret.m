function y = ret(X,type,pct)

tol = 1e-8;
[m,n] = size(X);
y = X(2:end,:)./(X(1:end-1,:) + tol);

if strcmp(type,'simple') %simple/arithmetic returns
    y = y - 1;
elseif strcmp(type,'log') %log/ln/continuously compounded returns
    y = log(y + tol);
else
   error('''type'' must be either ''simple'' or ''log''');
end

y = [NaN(1,n); y];
if pct
    y = 100*y;
end

end