/*
 * ChainBuilder ESB
 * 		Visual Enterprise Integration
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
 * $Id: IComponentProcessor.java,v 1.1.1.1 2007/04/09 17:49:28 mpreston Exp $
 */

package com.bostechcorp.cbesb.runtime.ccsl.jbi.messaging;

import javax.jbi.messaging.MessageExchange;

/**
 * The interface class for component processor classes for ChainBuilder ESB components. 
 * It defines a set of API used by consumer processor or provider processor.
 * 
 * The implementation class only need to implement the method which is relevant depending on where it 
 * is consumer or provider.
 * 
 * 
 * @author elu
 *
 */
public interface IComponentProcessor {
	
	/** 
	 * The start() method is called when starting a component. It is used for Consumer endpoint.
	 * 
	 * @throws Exception
	 */
    void start() throws Exception;

    /**
     * The stop() s called when starting a component. It is used for Consumer endpoint.
     * @throws Exception
     */
    void stop() throws Exception;

    /**
     * Process the RobustInOnly MessageExchange. Refer to Page 32 in the JBI spec.
     * It is applicable for provider endpoint.
     * 
     * @param exchange
     * @throws Exception
     */
	void processRobustInOnly(MessageExchange exchange) throws Exception;
	
	/**
	 * Process the InOnly MessageExchange. Refer to Page 31 in the JBI spec.
	 * It is applicable for provider endpoint.
	 * 
	 * @param exchange
	 * @throws Exception
	 */
	void processInOnly(MessageExchange exchange) throws Exception;
	
	/**
	 * 
	 * Process the InOut and In OptionalOut MessageExchange Pattern. Refer to Page 32 in the JBI spec.
	 * It is applicable for provider endpoint.
	 * 
	 * @param exchange
	 * @param b
	 * @throws Exception
	 */
	void processInOut(MessageExchange exchange, boolean b) throws Exception;
	
	/**
	 * The process() method is called from the child consumer processor when it read data from external connection and 
     * turn it into an MessageExchange and route to NMR. 
     * It is applicable for provider endpoint.
	 * 
	 * @param message
	 * @throws Exception
	 */
			
	void process (Object message) throws Exception;
    
}
