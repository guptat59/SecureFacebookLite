Project              : Facebook API
Implementations      : HomePage, Post, Friend List, Profile, Picture, Album
Implemented by       : Rakesh Dammalapati [29938403]
                       Tarun Gupta Akirala []
Statistics reference : https://zephoria.com/top-15-valuable-facebook-statistics/


What is working? 
All the above mentioned API's are working. 

Largest user set tried on?
Created 10,000 users and performed varies api operations.

Simulator Statistics?
From the above source, it is understood that there are 1.5billion total monthly users and 1billion active users in a day. It is also mentioned that 600 million posts and 300 million photos are uploaded per day and 5 new profiles are created every min. Using this information we have narrowed down on the following assumptions and implemented our schedulers accordingly.

     * We have scaled down all the statistics to one hour time period.
     * Establishing a relation between all of them it can be said that,
       1) active users = 2/3 * totalusers
       2) num of posts in an hour = 3/5 * active users
       3) num of photo in an hour = 1/2 * num of  posts
       4) num of new users per min = 1 or numofusers * 0.000005 whichever is higher
       5) We made the following assumptions based on other observations,
          => that atmost 25% of active users are active in posting
          => 40% of active users are involved in posting photos and posts
          => One new Friend connection is made every minute. 
          => All the other users are just viewing the info.
          => 1 new profile is created per min.
       6) Scheduler time for posts = 1 / (numof posts in an hour / (60*60))
       7) Scheduler time for photos = 2 * sctime for posts
       8) scheduler time for new profile = 60sec/numofnewusers per min 
       9) scheduler time for new friend request = 60secs
       10) scheduler time for viewing profiles, home page, albums = 0.01 sec
      
      Ideal case, 
      	1) TotalUsers = 10,000
      	2) Active Users = 6666
	3) num of posts = 4000 /hr or 70 /min
	4) num of photos = 2000 /hr or 35 /min
	5) scheduler time for posts = 0.86 sec
	6) scheduler time for images = 1.6 sec