function [beta,s2,aic,bic,ll,u] = wls(X,y,w)
    epsilon1 = 1e-8;
    epsilon2 = 0;
    
    [T,k] = size(X);
    if isempty(w)
        W = 1;
    else
        W = sparse(diag(w));
    end
    if numel(X) > 10000 %T > 10000
        [L,p] = chol(X'*W*X + epsilon1*eye(k));
        if p > 0
            error('X''*W*X is singular'); 
        else
            L = sparse(L);
            L = (L\(L'\(X'*W)));
            beta = L*y;
            %beta = L\(L'\(X'*W*y));
            %L = (L\(L'\(X'*W)));
        end
    else
        L = (X'*W*X + epsilon1*speye(k))\X'*W;
        beta = L*y;
        %beta = (X'*W*X + eps*speye(k))\X'*W*y;
        %L = (X'*W*X + eps*speye(k))\X'*W;
    end

    %
    np = k + 1;
    u = y - X*beta;
    uwu = u' * W * u;
    s2 = uwu/(T-np); %this is the OLS estimate, NOT the MLE estimate which is equal to uu/T! https://en.wikipedia.org/wiki/Ordinary_least_squares
    ll = -0.5*T*(log(2*pi) + log(s2 + epsilon2)) - 0.5*uwu/(s2 + epsilon2); 
    aic = 2*np - 2*ll;
    bic = np*log(T) - 2*ll;
end