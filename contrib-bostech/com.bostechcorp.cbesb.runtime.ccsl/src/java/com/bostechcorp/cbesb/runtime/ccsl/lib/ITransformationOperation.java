/*
 * ChainBuilder ESB
 *          Visual Enterprise Integration
 * 
 * Copyright (C) 2006 Bostech Corporation
 * 
 * This program is free software; you can redistribute it and/or modify it 
 * under the terms of the GNU General Public License as published by the 
 * Free Software Foundation; either version 2 of the License, or (at your option) 
 * any later version.
 *
 * This program is distributed in the hope that it will be useful, 
 * but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
 * or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License 
 * for more details.
 * 
 * You should have received a copy of the GNU General Public License along with 
 * this program; if not, write to the Free Software Foundation, Inc., 
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 *
 *
 * $Id: ITransformationOperation.java,v 1.1.1.1 2007/04/09 17:49:29 mpreston Exp $
 */
package com.bostechcorp.cbesb.runtime.ccsl.lib;

import java.util.Map;


/*
 * This is the interface to implement for transformer "class" and "user" operations
 */
public interface ITransformationOperation {
	/*
	 * NOTE: implementing classes must have a default constructor.
	 */

	public void addProperty( String name, String value );
		/*
		 * This is called once for each property immediately after the class is instantiated. The class should save these settings to member variables.
		 */

// implement propertylist later
//	public void addPropertyList( PropertyList ); 
		/*
		 * This is called once for each propertylist immediately after the class is instantiated. The class should save these settings to member variables.
		 */

	public void initialize(Map<String, Object>transformerContext) throws Exception;
		/*
		 * This is called once before each message is transformed, the class should initialize variables, etc.
		 */
	
	
	public void cleanup(Map<String, Object>transformerContext) throws Exception;
		/*
		 * This is called once after each message is transformed. Clean up any resources
		 */
	
	public boolean process(String[] sources, String[] targets) throws Exception;
		/*
		 * This is called to perform the operation. Return false to skip
		 * target processing after the operation completes.
		 */
}
