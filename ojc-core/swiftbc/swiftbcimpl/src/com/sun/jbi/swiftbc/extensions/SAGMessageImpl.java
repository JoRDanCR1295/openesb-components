/************************************************************************************
 *
 *   Copyright © 2006 Sun Microsystems, Inc., 4150 Network Circle, Santa Clara,
 *   California 95054, U.S.A. All rights reserved. Sun Microsystems, Inc. has
 *   intellectual property rights relating to technology embodied in the product
 *   that is described in this document. In particular, and without limitation,
 *   these intellectual property rights may include one or more of the U.S. patents
 *   listed at http://www.sun.com/patents and one or more additional patents or
 *   pending patent applications in the U.S. and in other countries. THIS PRODUCT
 *   CONTAINS CONFIDENTIAL INFORMATION AND TRADE SECRETS OF SUN MICROSYSTEMS, INC.
 *   USE, DISCLOSURE OR REPRODUCTION IS PROHIBITED WITHOUT THE PRIOR EXPRESS WRITTEN
 *   PERMISSION OF SUN MICROSYSTEMS, INC.U.S. Government Rights - Commercial
 *   software.  Government users are subject to the Sun Microsystems, Inc. standard
 *   license agreement and applicable provisions of the FAR and its supplements.
 *   Use is subject to license terms.  This distribution may include materials
 *   developed by third parties. Sun, Sun Microsystems, the Sun logo, Java
 *   Composite Application Platform Suite,  SeeBeyond, eGate, eInsight, eVision, eTL,
 *   eXchange, eView, eIndex, eBAM and  eWay are trademarks or registered trademarks of
 *   Sun Microsystems, Inc. in the U.S. and other countries. All SPARC trademarks are
 *   used under license and are trademarks or registered trademarks of SPARC
 *   International, Inc. in the U.S. and other countries. Products bearing SPARC
 *   trademarks are based upon architecture developed by Sun Microsystems, Inc.
 *   UNIX is a registered trademark in the U.S. and other countries, exclusively
 *   licensed through X/Open Company, Ltd. This product is covered and controlled by
 *   U.S. Export Control laws and may be subject to the export or import laws in
 *   other countries.  Nuclear, missile, chemical biological weapons or nuclear
 *   maritime end uses or end users, whether direct or indirect, are strictly
 *   prohibited.  Export or reexport to countries subject to U.S. embargo or to
 *   entities identified on U.S. export exclusion lists, including, but not limited
 *   to, the denied persons and specially designated nationals lists is strictly
 *   prohibited.
 *
 *************************************************************************************/
package com.sun.jbi.swiftbc.extensions;

import com.sun.jbi.swiftbc.*;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.swiftbc.extensions.SwiftEnvelope;
import com.sun.jbi.swiftbc.extensions.SwiftLetter;
import com.sun.jbi.swiftbc.extensions.SwiftMessage;
import com.sun.jbi.swiftbc.extensions.SwiftNamedItem;
import com.sun.jbi.swiftbc.extensions.SwiftNamedItemList;
import com.sun.jbi.swiftbc.extensions.SAGObjectFactory;
import com.sun.jbi.swiftbc.extensions.SAGObjectFactoryFactory;
import com.sun.jbi.swiftbc.extensions.SwiftRoutingList;
import com.sun.jbi.swiftbc.extensions.base.SwiftMessageBase;
import java.io.DataInput;
import java.io.DataOutput;
import java.util.logging.Level;
import java.util.logging.Logger;


/**
 * This class implements the SAGMessage interface for communicating
 * with SAG (Swift Alliance Gateway)
 *
 * @author Harry Liu (harry.liu@sun.com)
 * @version cvs revision: $Revision: 1.1 $ Last Modified: $Date: 2007/04/20 05:37:49 $
 */

public class SAGMessageImpl extends SwiftMessageBase 
        implements SwiftMessage/*, PersistableData*/ {
    private static SAGObjectFactory objectFactory = (new SAGObjectFactoryFactory()).getObjectFactory();
    /**
     *
     * @author Harry Liu (harry.liu@sun.com)
     * @version cvs revision: $Revision: 1.1 $   Last Modified: $Date: 2007/04/20 05:37:49 $
     */
    public class SAGEnvelopeImpl implements SwiftEnvelope {
        
        /**
         *
         * @author Harry Liu (harry.liu@sun.com)
         * @version cvs revision: $Revision: 1.1 $   Last Modified: $Date: 2007/04/20 05:37:49 $
         */
        public class SAGNamedItemListImpl implements SwiftNamedItemList {
            /**
             *
             * @author Harry Liu (harry.liu@sun.com)
             * @version cvs revision: $Revision: 1.1 $   Last Modified: $Date: 2007/04/20 05:37:49 $
             */
            public class SAGNamedItemImpl implements SwiftNamedItem {
                public static final String version = "cvs $Revision: 1.1 $   $Date: 2007/04/20 05:37:49 $";
                private Logger logger = Messages.getLogger(SAGNamedItemImpl.class);
                private String logMsg;
                
                private SwiftNamedItem jniItem;
                private SAGNamedItemListImpl childItemList;
                
                /**
                 * Constructor
                 *
                 */
                public SAGNamedItemImpl() {
                    setJniNamedItem(objectFactory.getNewNamedItem());
                    if (logger.isLoggable(Level.INFO)) {
                        logMsg = "SAGNamedItemImpl.SAGNamedItemImpl(): Constructing ...";
                        logger.log(Level.INFO,logMsg);
                    }
                }
                
                /**
                 * Constructor
                 *
                 */
                public SAGNamedItemImpl(SwiftNamedItem item) {
                    if (logger.isLoggable(Level.INFO)) {
                        logMsg = "SAGNamedItemImpl.SAGNamedItemImpl(): Constructing with NamedItem ["
                                + item
                                + "].";
                        logger.log(Level.INFO,logMsg);
                    }
                    
                    setJniNamedItem(item);
                    
                    //if (logger.isDebugEnabled()) {
                    //    logger.debug("SAGNamedItemImpl.SAGNamedItemImpl(): Done [" + this + "].");
                    //}
                }
                
                /**
                 * @return
                 */
                public SwiftNamedItem getJniNamedItem() {
                    return this.jniItem;
                }
                
                public void setJniNamedItem(SwiftNamedItem item) {
                    this.jniItem = item;
                }
                
                /**
                 * @see com.stc.connector.appconn.sag.SAGMessage.SAGNamedItemList.SAGNamedItem#setItemName(java.lang.String)
                 */
                public void setItemName(String name) {
                    this.jniItem.setItemName(name);
                }
                
                /**
                 * @see com.stc.connector.appconn.sag.SAGMessage.SAGNamedItemList.SAGNamedItem#setItemValue(java.lang.String)
                 */
                public void setItemValue(String value) {
                    this.jniItem.setItemValue(value);
                }
                
                /**
                 * @see com.stc.connector.appconn.sag.SAGMessage.SAGNamedItemList.SAGNamedItem#setNamedItemList(com.stc.connector.appconn.sag.SAGMessage.SAGNamedItemList)
                 */
                public void setNamedItemList(SwiftNamedItemList itemList) {
                    this.jniItem.setNamedItemList(((SAGNamedItemListImpl) itemList).getJniNamedItemList());
                }
                
                /**
                 * @see com.stc.connector.appconn.sag.SAGMessage.SAGNamedItemList.SAGNamedItem#getItemName()
                 */
                public String getItemName() {
                    return this.jniItem.getItemName();
                }
                
                /**
                 * @see com.stc.connector.appconn.sag.SAGMessage.SAGNamedItemList.SAGNamedItem#getItemValue()
                 */
                public String getItemValue() {
                    return this.jniItem.getItemValue();
                }
                
                /**
                 * @see com.stc.connector.appconn.sag.SAGMessage.SAGNamedItemList.SAGNamedItem#getNamedItemList()
                 */
                public SwiftNamedItemList getNamedItemList() {
                    if (null == this.childItemList) {
                        this.childItemList = new SAGNamedItemListImpl(this.jniItem.getNamedItemList());
                    }
                    return this.childItemList;
                }
                
                /**
                 * @see com.stc.connector.appconn.sag.SAGMessage.SAGNamedItemList.SAGNamedItem#isItemList()
                 */
                public boolean isItemList() {
                    return this.jniItem.isItemList();
                }
                
            }
            
            public static final String version = "cvs $Revision: 1.1 $   $Date: 2007/04/20 05:37:49 $";
            private Logger logger = Messages.getLogger(SAGNamedItemListImpl.class);
            private String logMsg;
            
            private SwiftNamedItemList jniItemList;
            
            /**
             * Constructor
             *
             */
            public SAGNamedItemListImpl() {
                setJniNamedItemList(objectFactory.getNewNamedItemList());
                if (logger.isLoggable(Level.INFO)) {
                    logMsg = "SAGNamedItemListImpl.SAGNamedItemListImpl(): Constructing ...";
                    logger.log(Level.INFO,logMsg);
                }
            }
            
            /**
             * Constructor
             *
             */
            public SAGNamedItemListImpl(SwiftNamedItemList itemList) {
                if (logger.isLoggable(Level.INFO)) {
                    logMsg = "SAGNamedItemListImpl.SAGNamedItemListImpl(): Constructing with NamedItemList ["
                            + itemList
                            + "].";
                    logger.log(Level.INFO,logMsg);
                }
                
                setJniNamedItemList(itemList);
                
                //if (logger.isDebugEnabled()) {
                //    logger.debug("SAGNamedItemListImpl.SAGNamedItemListImpl(): Done [" + this + "].");
                //}
            }
            
            /**
             * @return
             */
            public SwiftNamedItemList getJniNamedItemList() {
                return this.jniItemList;
            }
            
            public void setJniNamedItemList(SwiftNamedItemList itemList) {
                this.jniItemList = itemList;
            }
            
            /**
             * @see com.stc.connector.appconn.sag.SAGMessage.SAGNamedItemList#getNumberOfItem()
             */
            public int getNumberOfItem() {
                return this.jniItemList.getNumberOfItem();
            }
            
            /**
             * @see com.stc.connector.appconn.sag.SAGMessage.SAGNamedItemList#getNamedItem(int)
             */
            public SwiftNamedItem getNamedItem(int item) {
                return new SAGNamedItemImpl(this.jniItemList.getNamedItem(item));
            }
            
            /**
             * @see com.stc.connector.appconn.sag.SAGMessage.SAGNamedItemList#getNamedItem(java.lang.String)
             */
            public SwiftNamedItem getNamedItem(String itemName) {
                return new SAGNamedItemImpl(this.jniItemList.getNamedItem(itemName));
            }
            
            /**
             * @see com.stc.connector.appconn.sag.SAGMessage.SAGNamedItemList#insert(com.stc.connector.appconn.sag.SAGMessage.SAGNamedItemList.SAGNamedItem)
             */
            public void insert(SwiftNamedItem item) {
                this.jniItemList.insert(((SAGNamedItemImpl) item).getJniNamedItem());
            }

            public void destroy(int item) {
                throw new UnsupportedOperationException("Not supported yet.");
            }

            public void destroy(String itemName) {
                throw new UnsupportedOperationException("Not supported yet.");
            }
            
        }
        
        public static final String version = "cvs $Revision: 1.1 $   $Date: 2007/04/20 05:37:49 $";
        private Logger logger = Messages.getLogger(this.getClass());
        private String logMsg;
        
        private SwiftEnvelope jniEnvelope;
        private SAGNamedItemListImpl itemList;
        
        /**
         * Constructor
         *
         */
        public SAGEnvelopeImpl() {
            setJniEnvelope(objectFactory.getNewEnvelope());
            if (logger.isLoggable(Level.INFO)) {
                logMsg = "SAGEnvelopeImpl.SAGEnvelopeImpl(): Constructing ...";
                logger.log(Level.INFO,logMsg);
            }
        }
        
        /**
         * Constructor
         * @param envelope
         */
        public SAGEnvelopeImpl(SwiftEnvelope envelope) {
            if (logger.isLoggable(Level.INFO)) {
                logMsg = "SAGEnvelopeImpl.SAGEnvelopeImpl(): Constructing with Envelope ["
                        + envelope
                        + "].";
                logger.log(Level.INFO,logMsg);
            }
            
            setJniEnvelope(envelope);
            
            //if (logger.isDebugEnabled()) {
            //    logger.debug("SAGEnvelopeImpl.SAGEnvelopeImpl(): Done [" + this + "].");
            //}
        }
        
        /**
         * @return
         */
        public SwiftEnvelope getJniEnvelope() {
            return this.jniEnvelope;
        }
        
        /**
         * @param envelope
         */
        public void setJniEnvelope(SwiftEnvelope envelope) {
            this.jniEnvelope = envelope;
            
            this.itemList = new SAGNamedItemListImpl(this.jniEnvelope.getNamedItemList());
        }
        
        /**
         * @see com.stc.connector.sagadapter.sagapi.SAGMessage.SAGEnvelope#setApplicationId(java.lang.String)
         */
        public void setApplicationId(String value) {
            this.jniEnvelope.setApplicationId(value);
        }
        
        /**
         * @see com.stc.connector.sagadapter.sagapi.SAGMessage.SAGEnvelope#setContextId(java.lang.String)
         */
        public void setContextId(String value) {
            this.jniEnvelope.setContextId(value);
        }
        
        /**
         * @see com.stc.connector.sagadapter.sagapi.SAGMessage.SAGEnvelope#setMsgFormat(java.lang.String)
         */
        public void setMsgFormat(String value) {
            this.jniEnvelope.setMsgFormat(value);
        }
        
        /**
         * @see com.stc.connector.sagadapter.sagapi.SAGMessage.SAGEnvelope#setSender(java.lang.String)
         */
        public void setSender(String value) {
            this.jniEnvelope.setSender(value);
        }
        
        /**
         * @see com.stc.connector.sagadapter.sagapi.SAGMessage.SAGEnvelope#setSenderAuth(java.lang.String)
         */
        public void setSenderAuth(String value) {
            this.jniEnvelope.setSenderAuth(value);
        }
        
        /**
         * @see com.stc.connector.sagadapter.sagapi.SAGMessage.SAGEnvelope#setReceiver(java.lang.String)
         */
        public void setReceiver(String value) {
            this.jniEnvelope.setReceiver(value);
        }
        
        /**
         * @see com.stc.connector.sagadapter.sagapi.SAGMessage.SAGEnvelope#setLocalAuth(java.lang.String)
         */
        public void setLocalAuth(String value) {
            this.jniEnvelope.setLocalAuth(value);
        }
        
        /**
         * @see com.stc.connector.sagadapter.sagapi.SAGMessage.SAGEnvelope#setApplicationStatus(java.lang.String)
         */
        public void setApplicationStatus(String value) {
            this.jniEnvelope.setApplicationStatus(value);
        }
        
        /**
         * @see com.stc.connector.sagadapter.sagapi.SAGMessage.SAGEnvelope#getApplicationId()
         */
        public String getApplicationId() {
            return this.jniEnvelope.getApplicationId();
        }
        
        /**
         * @see com.stc.connector.sagadapter.sagapi.SAGMessage.SAGEnvelope#getContextId()
         */
        public String getContextId() {
            return this.jniEnvelope.getContextId();
        }
        
        /**
         * @see com.stc.connector.sagadapter.sagapi.SAGMessage.SAGEnvelope#getMsgFormat()
         */
        public String getMsgFormat() {
            return this.jniEnvelope.getMsgFormat();
        }
        
        /**
         * @see com.stc.connector.sagadapter.sagapi.SAGMessage.SAGEnvelope#getSender()
         */
        public String getSender() {
            return this.jniEnvelope.getSender();
        }
        
        /**
         * @see com.stc.connector.sagadapter.sagapi.SAGMessage.SAGEnvelope#getSenderAuth()
         */
        public String getSenderAuth() {
            return this.jniEnvelope.getSenderAuth();
        }
        
        /**
         * @see com.stc.connector.sagadapter.sagapi.SAGMessage.SAGEnvelope#getReceiver()
         */
        public String getReceiver() {
            return this.jniEnvelope.getReceiver();
        }
        
        /**
         * @see com.stc.connector.sagadapter.sagapi.SAGMessage.SAGEnvelope#getMsgRef()
         */
        public String getMsgRef() {
            return this.jniEnvelope.getMsgRef();
        }
        
        /**
         * @see com.stc.connector.sagadapter.sagapi.SAGMessage.SAGEnvelope#getLocalAuth()
         */
        public String getLocalAuth() {
            return this.jniEnvelope.getLocalAuth();
        }
        
        /**
         * @see com.stc.connector.sagadapter.sagapi.SAGMessage.SAGEnvelope#getApplicationStatus()
         */
        public String getApplicationStatus() {
            return this.jniEnvelope.getApplicationStatus();
        }
        
        /**
         * @see com.stc.connector.sagadapter.sagapi.SAGMessage.SAGEnvelope#getNamedItemList()
         */
        public SwiftNamedItemList getNamedItemList() {
            if (null == this.itemList) {
                this.itemList = new SAGNamedItemListImpl(this.jniEnvelope.getNamedItemList());
            }
            return this.itemList;
        }
        
        /**
         * @see com.stc.connector.sagadapter.sagapi.SAGMessage.SAGEnvelope#isApplicationStatusSuccess()
         */
        public boolean isApplicationStatusSuccess() {
            return SAGConstants.APPLICATION_STATUS_SUCCESS.equalsIgnoreCase(this.getApplicationStatus());
        }
        
        /**
         * @see com.stc.connector.sagadapter.sagapi.SAGMessage.SAGEnvelope#isApplicationStatusFailure()
         */
        public boolean isApplicationStatusFailure() {
            return SAGConstants.APPLICATION_STATUS_FAILURE.equalsIgnoreCase(this.getApplicationStatus());
        }
        
        /**
         * @see java.lang.Object#toString()
         */
        public String toString() {
            StringBuffer ret = new StringBuffer(super.toString());
            try {
                if (getApplicationId() != null && getApplicationId().length() > 0) {
                    ret.append(" ApplicationId [").append(getApplicationId()).append("]. ");
                }
                if (getApplicationStatus() != null && getApplicationStatus().length() > 0) {
                    ret.append(" ApplicationStatus [").append(getApplicationStatus()).append("]. ");
                }
                if (getContextId() != null && getContextId().length() > 0) {
                    ret.append(" ContextId [").append(getContextId()).append("]. ");
                }
                if (getReceiver() != null && getReceiver().length() > 0) {
                    ret.append(" Receiver [").append(getReceiver()).append("]. ");
                }
                if (getLocalAuth() != null && getLocalAuth().length() > 0) {
                    ret.append(" LocalAuth [").append("********").append("]. ");
                }
                if (getMsgFormat() != null && getMsgFormat().length() > 0) {
                    ret.append(" MsgFormat [").append(getMsgFormat()).append("]. ");
                }
                if (getMsgRef() != null && getMsgRef().length() > 0) {
                    ret.append(" MsgRef [").append(getMsgRef()).append("]. ");
                }
                if (getSender() != null && getSender().length() > 0) {
                    ret.append(" Sender [").append(getSender()).append("]. ");
                }
                if (getSenderAuth() != null && getSenderAuth().length() > 0) {
                    ret.append(" SenderAuth [").append("********").append("]. ");
                }
                if (getNamedItemList() != null && getNamedItemList().getNumberOfItem() > 0) {
                    ret.append(" NamedItemList has <").append(getNamedItemList().getNumberOfItem()).append("> items. ");
                }
            } catch (Exception e) {
                //ret = new StringBuffer(super.toString());
            }
            
            return ret.toString();
        }

        public long getPeer() {
            throw new UnsupportedOperationException("Not supported yet.");
        }
        
        public SwiftRoutingList getRoutingListRequest() {
            throw new UnsupportedOperationException("Not supported yet.");
        }

        public SwiftRoutingList getRoutingListResponse() {
            throw new UnsupportedOperationException("Not supported yet.");
        }
        
    }
    
    public static final String ver = "cvs $Revision: 1.1 $   $Date: 2007/04/20 05:37:49 $";
    private Logger logger = Messages.getLogger(this.getClass());
    private String logMsg;
    
    /**
     * Version ID for this class.  This is used for persisting this
     * object using the PersistableMessage interface.
     */
    private static int version = 1;
    
    private SwiftMessage swiftMessage;
    
    private SAGEnvelopeImpl envelope;
    private SwiftLetter letter;
    
    /**
     * Creates a new instance of SAGMessageImpl
     *
     */
    public SAGMessageImpl() {
        SwiftMessage msg = objectFactory.getNewMessage();
        msg.setEnvelope(objectFactory.getNewEnvelope());
        msg.setLetter(objectFactory.getNewLetter());
        setSwiftMessage(msg);
        if (logger.isLoggable(Level.INFO)) {
            logMsg = "SAGMessageImpl.SAGMessageImpl(): Constructing ...";
            logger.log(Level.INFO,logMsg);
        }
    }
    
    /**
     * Constructor
     * @param msg
     */
    public SAGMessageImpl(SwiftMessage msg) {
        if (logger.isLoggable(Level.INFO)) {
            logMsg = "SAGMessageImpl.SAGMessageImpl(): Constructing with JNI Message ["
                    + msg
                    + "].";
            logger.log(Level.INFO,logMsg);
        }
        
        setSwiftMessage(msg);
        
        //if (logger.isDebugEnabled()) {
        //    logger.debug("SAGMessageImpl.SAGMessageImpl(): Done [" + this + "].");
        //}
    }
    
    /**
     * @return
     */
    public SwiftMessage getSwiftMessage() {
        swiftMessage.setEnvelope(this.envelope.getJniEnvelope());
        SwiftLetter tl = objectFactory.getNewLetter();
        tl.clone(this.letter.getBuffer());
        swiftMessage.setLetter(tl);

        return this.swiftMessage;
    }
    
    /**
     * @param msg
     */
    public void setSwiftMessage(SwiftMessage msg) {
        this.swiftMessage = msg;
        
        
        this.envelope = new SAGEnvelopeImpl(this.swiftMessage.getEnvelope());
        this.letter = this.swiftMessage.getLetter();
    }
    
    //
    // Begin SAGMessage interface
    //
    
    /**
     * @see com.stc.connector.sagadapter.sagapi.SAGMessage#getEnvelope()
     */
    public SwiftEnvelope getEnvelope() {
        if (null == this.envelope) {
            this.envelope = new SAGEnvelopeImpl(this.swiftMessage.getEnvelope());
        }
        return this.envelope;
    }
    
    /**
     * @see com.stc.connector.sagadapter.sagapi.SAGMessage#setEnvelope(com.stc.connector.appconn.sag.SAGMessage.SAGEnvelope)
     */
    public void setEnvelope(SwiftEnvelope envelope) {
        this.envelope = (SAGEnvelopeImpl) envelope;
        this.swiftMessage.setEnvelope(this.envelope.getJniEnvelope());
    }
    
    /**
     * @see com.stc.connector.sagadapter.sagapi.SAGMessage#setLetter(SwiftLetter)
     */
    public void setLetter(SwiftLetter letter) {
        this.swiftMessage.setLetter(letter);
        this.letter = letter;
    }
    
    /**
     * @see com.stc.connector.sagadapter.sagapi.SAGMessage#getLetter()
     */
    public SwiftLetter getLetter() {
        return this.letter;
    }
    
    /**
     * @see java.lang.Object#toString()
     */
    public String toString() {
        StringBuffer ret = new StringBuffer(super.toString());
        try {
            if (getEnvelope() != null) {
                ret.append(" Envelope [").append(getEnvelope().toString()).append("]. ");
            }
            if (getLetter() != null) {
                ret.append(" Letter [").append(getLetter()).append("]. ");
            }
        } catch (Exception e) {
            //ret = new StringBuffer(super.toString());
        }
        
        return ret.toString();
    }
    
    //
    // End SAGMessage Interface
    //
    
    //
    // Begin PersistableData Interface
    //
    
    /**
     * Persists (serialize) this message.  Ignore the MsgRef field.
     *
     * @param   dos The DataOutputStream to which the Java Bean
     *              message is persisted.
     *
     * @throws  Exception upon error.
     */
    //TODO: not done yet
    public void persist(DataOutput dos) throws Exception {
        dos.writeInt(version);
        dos.writeUTF(this.getLetter().getBuffer());
        dos.writeUTF(this.getEnvelope().getApplicationId());
        dos.writeUTF(this.envelope.getContextId());
        dos.writeUTF(this.envelope.getMsgFormat());
        dos.writeUTF(this.envelope.getSender());
        dos.writeUTF(this.envelope.getSenderAuth());
        dos.writeUTF(this.envelope.getReceiver());
        dos.writeUTF(this.envelope.getLocalAuth());
        dos.writeUTF(this.envelope.getApplicationStatus());
    }
    
    /**
     * Restore (deserialize) this message. Ignore the MsgRef field.
     *
     * @param   dis The DataInputStream from which the Java Bean
     *              message is restored.
     *
     * @throws  Exception upon error.
     */
    //TODO: not done yet
    public void restore(DataInput dis) throws Exception {
        int tmpVersion = dis.readInt();
        if (tmpVersion == 1) {
//            this.setLetter(dis.readUTF());
            this.getEnvelope().setApplicationId(dis.readUTF());
            this.getEnvelope().setContextId(dis.readUTF());
            this.getEnvelope().setMsgFormat(dis.readUTF());
            this.getEnvelope().setSender(dis.readUTF());
            this.getEnvelope().setSenderAuth(dis.readUTF());
            this.getEnvelope().setReceiver(dis.readUTF());
            this.getEnvelope().setLocalAuth(dis.readUTF());
            this.getEnvelope().setApplicationStatus(dis.readUTF());
        } else {
            throw new Exception("Unknown version id, " + tmpVersion +
                    " for object, " + getClass().getName());
        }
        
    }

    public String getUseType() {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    public String getEncodingStyle() {
        throw new UnsupportedOperationException("Not supported yet.");
    }
    
    //
    // End PersistableData Interface
    //
    
}
