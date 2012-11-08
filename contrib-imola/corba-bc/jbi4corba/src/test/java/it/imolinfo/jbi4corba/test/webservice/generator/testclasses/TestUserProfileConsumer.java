 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.test.webservice.generator.testclasses;

public class TestUserProfileConsumer implements UserProfileConsumerCorbaInterfaceOperations {
    
    public UserProfile getUserProfile (String code) throws UserProfileException {
        if (code.equals("fault")) {           
            UserProfileException ex  = new UserProfileException();
            ex.setReason("myreason");
            ex.setUserCode("myuserCode");
            UserProfile user = new UserProfile();
            user.setAddress("via vai");
            user.setName("Marco");
            user.setAge(23);            
            ex.setProfiles(new UserProfile[] {user});                        
            throw ex;            
        }
        System.err.println("Received code: " + code);
        UserProfile userMarco = new UserProfile();
        userMarco.setName("Marco");
        userMarco.setAddress("via Selice");
        userMarco.setAge(31);                
        userMarco.setTestArray(new int[]{2,3});
        System.err.println("ClassLoader:" + userMarco.getClass().getClassLoader());
        
        return userMarco;
    }

	public int[] setUserProfileArray(UserProfile[] users)
			throws UserProfileException {
		UserProfile user1  =new UserProfile();
		UserProfile user2  =new UserProfile();
		
		user1.setName("Marco");
		user1.setName("via Selice");
		user1.setAge(31);
		
		user2.setName("Marco2");
		user2.setName("via Selice2");
		user2.setAge(32);
		
		//return users2;
		return new int[]{1,2};
		// return 2;
	}

}
