function semantic_distance = get_semantic_distance(concept1, concept2)
    % find semantic distance
    %{ 
    input is two 2x1 vectors for

                  |x1|              |x3|
        concept1: |  | and concept2:|  |
                  |x2|              |x4|

    where associations with color1 are (x1, x3) and associations with 
    color2 are (x2, x4).
    %}
    
    x1 = concept1(1); x3 = concept2(1);
    x2 = concept1(2); x4 = concept2(2);
    num = (x1 + x4) - (x2 + x3);
    denom = sqrt(sig(x1).^2 + sig(x2).^2 + sig(x3).^2 + sig(x4).^2);
    prob_dx_gt_zero = normcdf(num ./ denom);
    prob_dx_lt_zero = 1 - prob_dx_gt_zero;
    semantic_distance = abs(prob_dx_gt_zero - prob_dx_lt_zero);
end

% returns estimation of standard deviation
function my_sig = sig(xi)
    my_sig = max(1.4.*xi.*(1-xi), .1);
end