/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)EmailEventHandler.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.smtpbc.extservice.server;

import java.io.BufferedWriter;
import java.io.ByteArrayOutputStream;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.apache.mina.common.ByteBuffer;
import org.apache.mina.common.IoHandlerAdapter;
import org.apache.mina.common.IoSession;

import com.sun.jbi.internationalization.Messages;




/**
 *
 *
 * @author       Alexander Fung
 * @version      $Revision: 1.4 $
 *
 */
public class EmailEventHandler extends IoHandlerAdapter {

    private static final Logger log =
        Messages.getLogger(EmailEventHandler.class);
    
    private static final Messages mMessages =
        Messages.getMessages(EmailEventHandler.class);
    
    private Set mEmailListeners;
    private StateFactory mStateFactory;

    public EmailEventHandler() {
        mEmailListeners = new HashSet();
        mStateFactory = new StateFactory(mEmailListeners);
    }

    public void addEmailListener(final EmailListener listener) {
        mEmailListeners.add(listener);
    }

    public void removeEmailListener(final EmailListener listener) {
        mEmailListeners.remove(listener);
    }

    @Override
	public void sessionOpened( final IoSession session ) throws Exception
    {
    	log.log(Level.FINEST,mMessages.getString("EmailEventHandler.sessionOpened"));
        super.sessionOpened(session);

        // Create the initial state and store it in the session.  This is
        // so that we can present the user immediately with a welcome notice

        // Run the action for the initial state
        final State beginState = mStateFactory.createWelcomeState();
        try {
            beginState.performAction(new PrintWriter(new BufferedWriter( new OutputStreamWriter( new ServiceOutputStream(session)))));
        } catch (final Exception ex) {
            EmailEventHandler.log.severe(ex.toString());
        }

        // Store the initial state in the session
        session.setAttribute("currentState", beginState);
        session.setAttribute("currentInput", new ByteArrayOutputStream());
    }
    public void messageReceived(IoSession session, Object message) throws Exception{

    	log.log(Level.FINEST,mMessages.getString("EmailEventHandler.receivedMessage",message.toString()));
    	final byte[] bytes = message.toString().getBytes("US-ASCII");    	
    	ByteArrayOutputStream baos =
            (ByteArrayOutputStream)session.getAttribute("currentInput");
            session.setAttribute("currentInput", baos);
            baos.write(bytes);
            doWork(message.toString(),new ServiceOutputStream(session),session);
    }
    

    public void doWork(final String content, final OutputStream out1, final IoSession session) {
        final PrintWriter out =
            new PrintWriter(new BufferedWriter(new OutputStreamWriter(out1)));
        State currentState = (State)session.getAttribute("currentState");

        try {
            // Get our next state
            currentState = currentState.nextState(content);

            // Perform the action required by that State.
            currentState.performAction(out);
            
            session.setAttribute("currentState", currentState);
        } catch (final Exception ex) {
            ex.printStackTrace();
            log.log(Level.SEVERE,mMessages.getString("EmailEventHandler.ExceptionInDoWork",ex),ex);

        }
    } 

    private static class ServiceOutputStream extends OutputStream
    {
        private final IoSession session;
        
        public ServiceOutputStream( final IoSession session )
        {
            this.session = session;
        }

        @Override
		public void close()
        {
            session.close();
        }

        @Override
		public void flush()
        {
        }

        @Override
		public void write( final byte[] b, final int off, final int len )
        {
            final ByteBuffer buf = ByteBuffer.wrap( b, off, len );
            buf.acquire(); // prevent from being pooled.
            session.write(buf);
        }

        @Override
		public void write( final byte[] b )
        {
            final ByteBuffer buf = ByteBuffer.wrap( b );
            buf.acquire(); // prevent from being pooled.
            session.write(buf);
        }

        @Override
		public void write( final int b )
        {
            final ByteBuffer buf = ByteBuffer.allocate( 1 );
            buf.put( ( byte ) b );
            buf.flip();
            session.write(buf);
        }
    }
}
