## HTML() is used to escape formatting and inline code
labels <- list(
            ## widgets related to variable selection, transformation, and model specification
            contr=HTML("The following are comparisons that are possible given the predictor (X) variables you chose. If you pick too many comparisons you will have a complicated model with diminished sensitivity and over-fitted to the data. <i>The key is to focus on the comparisons that are most relevant to your hypothesis.</i> If there happen to be significant effects you haven't thought of, the model algorithm will add them back in later."),
            cvar="Right Censoring Indicator (if this is survival/time-to event data, otherwise leave this blank):",
            gvar="Grouping Variable. This is NOT one of your predictor (X) variables. This is a nuisance variable to correct for. It's especially important that you read the help popup for this one so you can make the right choice.",
            gvarmulti="Check here to indicate that you have more than one grouping variable.",
            rvar="Random Variables (you can leave them all selected and the app will narrow them down for you):",
            tvar="Do any of these variables represent the order in which the data were collected? Your data will be sorted in that order after first sorting by grouping variable, if there is one.",
            tvarmulti="Check here to indicate that you have more than one time/order variable.",
            svar_method="How would you like to center your variables?",
            svar="Which of these variables would you like to center?",
            ## widgets related to residuals
            zphrange="Use the left and right side of the slider below to move the vertical blue lines until they enclose the biggest area where the red line is inside the dotted black lines on all the plots to the right",
            trend="Please choose the best description for the gray points:",
            abstrend="Please choose the best description for the gray points:",
            nonlin="Is the fit of the dotted blue line to the gray points noticeably better than the fit of the solid red line to the gray points?",
            absnonlin="Is the fit of the dotted blue line to the gray points noticeably better than the fit of the solid red line to the gray points?",
            qqldir="Is the LEFT tail of this plot above or below the red line?",
            qqrdir="Is the RIGHT tail of this plot above or below the red line?",
            qqrange="Use the slider below to show where the left and right tails start to consistently deviate from the red line"
            );

labels <- sapply(names(labels),function(ii) span(labels[[ii]],helpbutton(ii)),simplify=F);
## browser();
conditions <- list();

static_choices <- list(
             trend=c(' ','Upward Trend','Highest in the Middle (Peak)','Lowest in the Middle (Valley)','Downward Trend','Approximately Flat','Other'),
             abstrend=c(' ','Upward Trend','Highest in the Middle (Peak)','Lowest in the Middle (Valley)','Downward Trend','Approximately Flat','Other'),
             nonlin=c(' ','Yes','No'),
             absnonlin=c(' ','Yes','No'),
             qqldir=c(' ','Above','Below'),
             qqrdir=c(' ','Above','Below')
             );

cat('\n\nDone loading text.R\n');
