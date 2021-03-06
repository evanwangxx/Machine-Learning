function [J grad] = nnCostFunction(nn_params, ...
                                   input_layer_size, ...
                                   hidden_layer_size, ...
                                   num_labels, ...
                                   X, y, lambda)
%NNCOSTFUNCTION Implements the neural network cost function for a two layer
%neural network which performs classification
%   [J grad] = NNCOSTFUNCTON(nn_params, hidden_layer_size, num_labels, ...
%   X, y, lambda) computes the cost and gradient of the neural network. The
%   parameters for the neural network are "unrolled" into the vector
%   nn_params and need to be converted back into the weight matrices. 
% 
%   The returned parameter grad should be a "unrolled" vector of the
%   partial derivatives of the neural network.
%

% Reshape nn_params back into the parameters Theta1 and Theta2, the weight matrices
% for our 2 layer neural network
Theta1 = reshape(nn_params(1:hidden_layer_size * (input_layer_size + 1)), ...
                 hidden_layer_size, (input_layer_size + 1));

Theta2 = reshape(nn_params((1 + (hidden_layer_size * (input_layer_size + 1))):end), ...
                 num_labels, (hidden_layer_size + 1));

% Setup some useful variables
m = size(X, 1);     %number of examples
         
% You need to return the following variables correctly 
J = 0;
Theta1_grad = zeros(size(Theta1));
Theta2_grad = zeros(size(Theta2));

% ====================== YOUR CODE HERE ======================
% Instructions: You should complete the code by working through the
%               following parts.
%
% Part 1: Feedforward the neural network and return the cost in the
%         variable J. After implementing Part 1, you can verify that your
%         cost function computation is correct by verifying the cost
%         computed in ex4.m
%
% Part 2: Implement the backpropagation algorithm to compute the gradients
%         Theta1_grad and Theta2_grad. You should return the partial derivatives of
%         the cost function with respect to Theta1 and Theta2 in Theta1_grad and
%         Theta2_grad, respectively. After implementing Part 2, you can check
%         that your implementation is correct by running checkNNGradients
%
%         Note: The vector y passed into the function is a vector of labels
%               containing values from 1..K. You need to map this vector into a 
%               binary vector of 1's and 0's to be used with the neural network
%               cost function.
%
%         Hint: We recommend implementing backpropagation using a for-loop
%               over the training examples if you are implementing it for the 
%               first time.
%
% Part 3: Implement regularization with the cost function and gradients.
%
%         Hint: You can implement this around the code for
%               backpropagation. That is, you can compute the gradients for
%               the regularization separately and then add them to Theta1_grad
%               and Theta2_grad from Part 2.
%

% part 1: forward NN
X = [ones(m,1) X];
a_2 = [ones(m,1) sigmoid(X*Theta1')];
hThetaX = sigmoid(a_2 * Theta2');   % 5000*10
%recode the labels as vector

yvec = zeros(m,num_labels);
for i = 1: num_labels
    yvec(:,i) = y ==i;      % 5000*10
end
    
J = sum(sum(-yvec.*log(hThetaX) - (1-yvec).*log(1-hThetaX)));
J = J/m;

R = (lambda/(2*m))*(sum(sum((Theta1(:,2:size(Theta1,2)).^2)))+...
    sum(sum(Theta2(:,2:size(Theta2,2)).^2)));
J = J + R;

% =========================================================================

% Unroll gradients
a_1 = X;
a_3 = hThetaX;

d3 = a_3 - yvec;
d2 = d3*Theta2(:,2:size(Theta2,2)) .* sigmoidGradient(X*Theta1');

Theta2_grad = (1/m)*(Theta2_grad+ d3' * a_2);
Theta1_grad = (1/m)*(Theta1_grad+ d2' * a_1);

Theta2_grad = Theta2_grad + lambda/m * [zeros(size(Theta2,1),1)...
    Theta2(:,2:size(Theta2,2))];
Theta1_grad = Theta1_grad + lambda/m * [zeros(size(Theta1,1),1)...
    Theta1(:,2:size(Theta1,2))];

grad = [Theta1_grad(:) ; Theta2_grad(:)];


end




%for i = 1:m
%    iniy = zeros(num_labels, 1);
%    iniy(y(i)) = 1;
%    J = J + (-iniy'*(log(sigmoid(hThetaX(i,:))))'-...
%        ((1-iniy)')*(log(1-sigmoid(hThetaX(i,:))))');
%end


%J = 0;
%for i = 1:10
%    yvec = zeros(num_labels,1);
%    yvec(y(i)) = 1;
%    
%    costperex = -yvec' .* log(sigmoid(hThetaX(i,:))) - ...
%        ((1 - yvec').*log(1-sigmoid(hThetaX(i,:))))
%    
%    J = J + costperex
%    
%end
%J = sum(J);
%J = J/m