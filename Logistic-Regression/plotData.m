function plotData(X, y)
%PLOTDATA Plots the data points X and y into a new figure 
%   PLOTDATA(x,y) plots the data points with + for the positive examples
%   and o for the negative examples. X is assumed to be a Mx2 matrix.

% Create New Figure
figure; hold on;

% ====================== YOUR CODE HERE ======================
% Instructions: Plot the positive and negative examples on a
%               2D plot, using the option 'k+' for the positive
%               examples and 'ko' for the negative examples.
%
% =========================================================================
pos = find(y == 1);
neg = find(y == 0);

P1 = X(pos, 1);
P2 = X(pos, 2);
N1 = X(neg, 1);
N2 = X(neg, 2);

plot(P1, P2, 'r+')
plot(N1, N2, 'Bo')








hold off;

end
