function g = sigmoid(z)
%SIGMOID Compute sigmoid functoon
%   J = SIGMOID(z) computes the sigmoid of z.

% You need to return the following variables correctly 
g = zeros(size(z));

% ====================== YOUR CODE HERE ======================
% Instructions: Compute the sigmoid of each value of z (z can be a matrix,
%               vector or scalar).

s = size(zeros(size(z)));
for i = 1: s(2)
    for j = 1 : s(1)
        g(j, i) = 1/ (1 + (exp(1))^(-z(j , i)));
    end
   
end


% =============================================================

end
