 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/

package it.imolinfo.jbi4corba.utils;

import java.util.StringTokenizer;


public class HelperStringUtils {
	
	/**
	 * Method that extract string from 2 delimiter start and end passed as String
	 * 
	 * @param target
	 * @param start delimiter
	 * @param ends delimiters 
	 * @return the result String 
	 * */
	public static String ExtractString(final String  target,final String start,final String endWith ) {
		// TODO Auto-generated method stub
		   
		String result= null; 
		String currentToken=null;
		StringTokenizer tokenizer=new StringTokenizer(target,start);
		while(tokenizer.hasMoreTokens()){
			if((currentToken=tokenizer.nextToken()).contains(endWith)){			
			result=currentToken;
			}		
		}
		int index=result.indexOf(endWith);
		return result.substring(0, index);
		
	}
	
	/**
	 * Method that compare the two String a return if there are equals and ignore the String order
	 * 
	 * @param str1 String source
	 * @param str2 String to compare
	 * @return boolean true if the String are equals false if the String not are equals
	 * */
	public static boolean compareStringNoOrder(String str1,String str2){
			
		StringTokenizer tk=new StringTokenizer(str2);
		boolean sameSize=false;
		if(str1.length()==str2.length())
			sameSize=true;
		boolean equals=true;
		while(tk.hasMoreTokens()){
			if(!str1.contains(tk.nextToken())){
				equals=false;
				break;
			}
			
		}
		return equals;
		
	}
        
       

}
