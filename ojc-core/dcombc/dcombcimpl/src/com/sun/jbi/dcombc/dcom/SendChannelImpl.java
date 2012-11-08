/* *************************************************************************
 *
 *          Copyright (c) 2005, Sun Microsystems,
 *          All Rights Reserved
 *
 *          This program, and all the routines referenced herein,
 *          are the proprietary properties and trade secrets of
 *          Sun Microsystems.
 *
 *          Except as provided for by license agreement, this
 *          program shall not be duplicated, used, or disclosed
 *          without  written consent signed by an officer of
 *          Sun Microsystems.
 *
 ***************************************************************************/

package com.sun.jbi.dcombc.dcom;

import java.util.logging.Logger;
import java.util.logging.Level;

import com.sun.jbi.internationalization.Messages;

import com.sun.jbi.dcombc.Endpoint;
import com.sun.jbi.dcombc.DCOMException;
import com.sun.jbi.dcombc.util.DCOMUtil;
import com.sun.jbi.dcombc.extensions.DCOMOperation;
import com.sun.jbi.dcombc.extensions.DCOMInput;
import com.sun.jbi.dcombc.extensions.DCOMOutput;

import javax.jbi.component.ComponentContext;

import java.net.UnknownHostException;

import org.jinterop.dcom.common.JIException;
import org.jinterop.dcom.common.JISystem;
import org.jinterop.dcom.core.IJIComObject;
import org.jinterop.dcom.core.JICallObject;
import org.jinterop.dcom.core.JIComServer;
import org.jinterop.dcom.core.JIFlags;
import org.jinterop.dcom.core.JIPointer;
import org.jinterop.dcom.core.JIProgId;
import org.jinterop.dcom.core.JISession;
import org.jinterop.dcom.core.JIString;
import org.jinterop.dcom.core.JIVariant;
import org.jinterop.dcom.win32.ComFactory;
import org.jinterop.dcom.win32.IJIDispatch;


/**
 * Class representing exact channel for sending message to 
 * microsoft message queue.
 * 
 * @author Chandrakanth Belde
 */
public class SendChannelImpl implements Channel {

    private static final Messages mMessages = Messages.getMessages(SendChannelImpl.class);

    private static final Logger mLogger = Messages.getLogger(SendChannelImpl.class);

    private ComponentContext mContext;

    private Endpoint mEndpoint;

    private DCOMConnInfo dcomConnInfo;
    
    private DCOMOperation mOperation;

    private DCOMInput mInput;
    
    private DCOMOutput mOutput;
    
    private boolean isOpened;

    private JIComServer comStub = null;
	
    private IJIDispatch dispatch = null;
	
    private IJIComObject unknown = null; 

	/**
     * Creates a new instance of Channel
     */
    public SendChannelImpl(Endpoint endpoint, 
                           DCOMOperation operation, 
    			   ComponentContext context) {
        mEndpoint = endpoint;
        mContext = context;
        mOperation = operation;
        mInput = operation.getDCOMOperationInput();
        mOutput = operation.getDCOMOperationOutput();
        dcomConnInfo = new DCOMConnInfo();
        dcomConnInfo.setInterface(endpoint.getDCOMBinding().getUUID());
        dcomConnInfo.setMethod(mOperation.getMethod());
        dcomConnInfo.setDomain(endpoint.getDCOMAddress().getDomain());
        dcomConnInfo.setServer(endpoint.getDCOMAddress().getServer());
        dcomConnInfo.setUserName(endpoint.getDCOMAddress().getUsername());
        dcomConnInfo.setPassword(endpoint.getDCOMAddress().getPassword());
    }

    /**
	 * 
	 * @return
	 */
    public DCOMConnInfo getDCOMConnInfo(){
    	return this.dcomConnInfo;
    }
    
    /**
     * 
     */
    public Endpoint getEndpoint() {
        return mEndpoint;
    }

    /**
     * 
     */
    public DCOMOperation getDCOMOperation() {
        return mOperation;
    }
    
    /**
     * 
     */
    public DCOMInput getDCOMOperationInput() {
        return mInput;
    }
    
    /**
     * 
     */
    public DCOMOutput getDCOMOperationOutput() {
        return mOutput;
    }
    
    /**
     * to do 
     */
    public String receive(){
    // to do 
        return "string";
    }
    
    /**
     * Establishes the physical connection to the underlying system.
     *
     * @throws application specific DCOMException upon error.
     */
    synchronized public void open() throws DCOMException {
        if (!isOpened) {
        	try{
        		
        		//setAutoRegisteration true to register the dll so that it wont get exception
        		JISystem.setAutoRegisteration(true);
        		JISession session = JISession.createSession(dcomConnInfo.getServer(),
        													dcomConnInfo.getUserName(),
        													dcomConnInfo.getPassword());
                mLogger.log(Level.INFO, "SendChannelImpl.SESSION_CREATED");
                
        		comStub = new JIComServer(JIProgId.valueOf(session,
        												   dcomConnInfo.getInterface()),
        												   dcomConnInfo.getDomain(),
        												   session);
            	unknown = comStub.createInstance();
        		dispatch = (IJIDispatch)ComFactory.createCOMInstance(ComFactory.IID_IDispatch,unknown);
        		
                mLogger.log(Level.INFO, "SendChannelImpl.STUB_CREATED");
                
                isOpened = true;

            } catch (Exception ex) {
                mLogger.log(Level.INFO, "SendChannelImpl.CONNECT_SERVER_FAILED", 
                			new Object[] { dcomConnInfo.getDomain(),
				                		   dcomConnInfo.getServer(),
				                		   dcomConnInfo.getInterface(),
				                		   dcomConnInfo.getMethod()});
                throw new DCOMException(DCOMUtil.getStackTraceAsString(ex));
            }
            mLogger.log(Level.INFO, "SendChannelImpl.CONNECT_SUCCESS", 
        			new Object[] { dcomConnInfo.getDomain(),
		                		   dcomConnInfo.getServer(),
		                		   dcomConnInfo.getInterface(),
		                		   dcomConnInfo.getMethod()});
            
        }
    }

    /**
     * 
     */
    public String invoke(String inputValue) throws DCOMException {

    	String retVal =  null;
    	
        if (!isOpened) {
            mLogger.log(Level.INFO, "SendChannelImpl.INVOKE_CONNECT", 
            			new Object[] { dcomConnInfo.getDomain(),
					         		   dcomConnInfo.getServer(),
					         		   dcomConnInfo.getInterface(),
					         		   dcomConnInfo.getMethod()});

            open();
        }
        if (isOpened) {
        	try{
    		
    		Object results[] = dispatch.callMethodA(dcomConnInfo.getMethod(),
    				new Object[]{new JIVariant(new JIString(inputValue),true),new JIVariant(new JIString(""),true)});
    		
    		JIVariant variant = (JIVariant)results[2];
    		
    		retVal = variant.getObjectAsString().getString();
    		
    		
            } catch (Exception e) {
                throw new DCOMException("Fail to invoke method: " + dcomConnInfo.getMethod()+ "\n" + DCOMUtil.getStackTraceAsString(e));
            }
            mLogger.log(Level.INFO, "SendChannelImpl.INVOKE_SUCCESS",
	        			new Object[] { dcomConnInfo.getDomain(),
					         		   dcomConnInfo.getServer(),
					         		   dcomConnInfo.getInterface(),
					         		   dcomConnInfo.getMethod()});

        }
        return retVal;
    }

    /**
     * Closes(destroys) the physical connection to the underlying system.
     *
     * @throws application specific DCOMException upon error.
     */
    synchronized public void close() throws DCOMException {
        mLogger.log(Level.INFO, "SendChannelImpl.CLOSEOUT_CALLED");
        String msg;
        if (isOpened) {
        	try {
        		JISession.destroySession(dispatch.getAssociatedSession());
            } catch (Exception ex) {
                isOpened = false;
                throw new DCOMException(ex);
            }
            mLogger.log(Level.INFO, "SendChannelImpl.CLOSE_SUCCESS", 
        			new Object[] { dcomConnInfo.getDomain(),
				         		   dcomConnInfo.getServer(),
				         		   dcomConnInfo.getInterface(),
				         		   dcomConnInfo.getMethod()});

        } else {
            mLogger.log(Level.INFO, "SendChannelImpl.CLOSE_ALREADY",
        			new Object[] { dcomConnInfo.getDomain(),
		                		   dcomConnInfo.getServer(),
		                		   dcomConnInfo.getInterface(),
		                		   dcomConnInfo.getMethod()});
            isOpened = false;
        }
    }
}
