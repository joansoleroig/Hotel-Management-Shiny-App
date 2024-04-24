# Team A1 - Shiny App Deployment README

## Business Case

Kempinski Hotels, known for its unparalleled guest services, currently faces challenges in harnessing the full potential of its vast data to drive strategic decision-making. The absence of a centralized platform for data analysis has led to potential gaps in optimizing occupancy rates, managing revenue, and capitalizing on market dynamics. Furthermore, without predictive analytics, the ability to anticipate and mitigate booking cancellations remains untapped, which can lead to missed opportunities for maximizing room utilization and revenue.

Our Shiny application addresses these challenges by introducing a powerful internal tool that consolidates key performance metrics and predictive insights into a single, intuitive interface. By providing tracking of occupancy and revenue, the hotel management can swiftly respond to emerging trends and make data-informed decisions. The application’s predictive capabilities for cancellation probabilities empower the staff to proactively confirm risky bookings or consider strategic overbooking to ensure high occupancy levels.

Additionally, the pricing recommendation module within the app uses sophisticated algorithms to suggest optimal pricing for new bookings, balancing competitiveness with profitability. This ensures that Kempinski Hotels can maintain its market-leading position by offering rates that reflect both guest value and business strategy.

Our solution transforms data into a strategic asset, positioning Kempinski Hotels at the forefront of innovation in the hospitality industry, ready to meet the evolving demands of its clientele while enhancing operational efficiency and revenue management.

## Notes

1. **Initial Model Training**: The first run of the Shiny app might take a longer time as it involves model training. This is essential for predictive analytics. Allow the process to complete. Subsequent reloadings of the shiny app will not take a long time as the shiny app will refer to the trained images of the machine learning models. 

2. **Access the App**: Once the server has restarted, you can access your Shiny app by navigating to your server's IP address or domain name followed by the port number (usually 3838). For example:
   http://your_server_ip:3838/myapp

3. **Usage**: After the initial training, subsequent runs of the app will access the pre-trained models and focus on the Shiny app functionality. 
	- Dashboard: uses historical hotel booking data to provide users and hotel managers with typical hospitality KPIs that aggregate hotel performance over time. Filters use unique values for each relevant feature, and will therefore not interpolate and extrapolate data within time series.

	- Cancellation Prediction: uses a trained random forest classification machine learning model to predict a potential booking's cancellation probability. Can be used by room and hotel managers to determine overbooking strategy.

	- Pricing Recommendation: uses a trained logistic regression machine learning model to predict a potential booking's optimal price. Can be used by room and hotel managers to tailor dynamic pricing and revenue management strategies to optimise to a customer segment's highest willingness to pay.
