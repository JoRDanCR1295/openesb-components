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
 */

/*
 * @(#)QueueAccessOptions.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.mqbc.extservices;

import java.io.BufferedWriter;
import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import java.util.StringTokenizer;

import com.ibm.mq.MQC;

/**
 * The integer 'openOptions' parameter for the QM.accessQueue is actually a
 * series of bitflags and needed to be flattened out much like the
 * GetMessageOptions and PutMessageOptions. The purpose of this class, then, is
 * to provide a holding area for these flags and to expose a series of set/clear
 * methods for them.
 * 
 * @author rchen
 * @author Noel.Ang@sun.com 
 */
public final class QueueAccessOptions {
    /**
     * The options member variable is what will eventually be passed to the
     * accessQueue method on the QM. It is where the flags are stored as the
     * user calls the setXXX functions.
     */
    private int options;
    
    private final Set<OptionsVisitor> optionsVisitors;
    private final Set<OptionSerializerVisitor> optionsSerializerVisitors;


    /**
     * Create a QueueAccessOptions object.
     */
    public QueueAccessOptions() {
        optionsClearAll();
        
        Set<OptionsVisitor> visitors = new HashSet<OptionsVisitor>();
        visitors.add(new BindOpenOptionsVisitor());
        visitors.add(new InputOpenOptionsVisitor());
        optionsVisitors = Collections.unmodifiableSet(visitors);
        
        Set<OptionSerializerVisitor> serializers = new HashSet<OptionSerializerVisitor>();
        serializers.add(new BindOpenOptionsSerializer());
        serializers.add(new InputOpenOptionsSerializer());
        optionsSerializerVisitors = Collections.unmodifiableSet(serializers);
    }
    
    public QueueAccessOptions(int initialOptions) {
        this();
        options = initialOptions;
        if (initialOptions < 0) {
            throw new IllegalArgumentException();
        }
    }

    /**
     * Access to the entire bitfield value represented by this object.
     * 
     * @return int value corresponding to the options bit pattern.
     */
    public int getOptions() {
        return options;
    }

    /**
     * Clear all option flags.
     */
    public void optionsClearAll() {
        options = 0;
    }

    /**
     * Set/unset the MQOO_INPUT_AS_Q_DEF option.
     * 
     * @param bSet <code>True</code> to set the option.
     * 
     * @see com.ibm.mq.MQC#MQOO_INPUT_AS_Q_DEF
     */
    public void setMQOO_INPUT_AS_Q_DEF(boolean bSet) {
        if (bSet) {
            options |= MQC.MQOO_INPUT_AS_Q_DEF;
        } else {
            options &= ~MQC.MQOO_INPUT_AS_Q_DEF;
        }
    }
    
    public boolean getMQOO_INPUT_AS_Q_DEF() { 
       if ( (options & MQC.MQOO_INPUT_AS_Q_DEF) != 0) 
           return true;
       else
           return false;
         
    }

    /**
     * Set/unset the MQOO_INPUT_SHARED option.
     * 
     * @param bSet <code>True</code> to set the option.
     * 
     * @see com.ibm.mq.MQC#MQOO_INPUT_SHARED
     */
    public void setMQOO_INPUT_SHARED(boolean bSet) {
        if (bSet) {
            options |= MQC.MQOO_INPUT_SHARED;
        } else {
            options &= ~MQC.MQOO_INPUT_SHARED;
        }
    }
    
    public boolean getMQOO_INPUT_SHARED() {
        if ( (options & MQC.MQOO_INPUT_SHARED) != 0) 
           return true;
       else
           return false;
    }

    /**
     * Set/unset the MQOO_INPUT_EXCLUSIVE option.
     * 
     * @param bSet <code>True</code> to set the option.
     * 
     * @see com.ibm.mq.MQC#MQOO_INPUT_EXCLUSIVE
     */
    public void setMQOO_INPUT_EXCLUSIVE(boolean bSet) {
        if (bSet) {
            options |= MQC.MQOO_INPUT_EXCLUSIVE;
        } else {
            options &= ~MQC.MQOO_INPUT_EXCLUSIVE;
        }
    }
    
     public boolean getMQOO_INPUT_EXCLUSIVE() {  
          if ( (options & MQC.MQOO_INPUT_EXCLUSIVE) != 0) 
           return true;
       else
           return false;
          
      
    }

    /**
     * Set/unset the MQOO_BROWSE option.
     * 
     * @param bSet <code>True</code> to set the option.
     * 
     * @see com.ibm.mq.MQC#MQOO_BROWSE
     */
    public void setMQOO_BROWSE(boolean bSet) {
        if (bSet) {
            options |= MQC.MQOO_BROWSE;
        } else {
            options &= ~MQC.MQOO_BROWSE;
        }
    }
    
     public boolean getMQOO_BROWSE() {
          if ( (options & MQC.MQOO_BROWSE) != 0) 
           return true;
       else
           return false;
    }

    /**
     * Set/unset the MQOO_OUTPUT option.
     * 
     * @param bSet <code>True</code> to set the option.
     * 
     * @see com.ibm.mq.MQC#MQOO_OUTPUT
     */
    public void setMQOO_OUTPUT(boolean bSet) {
        if (bSet) {
            options |= MQC.MQOO_OUTPUT;
        } else {
            options &= ~MQC.MQOO_OUTPUT;
        }
    }
    
     public boolean getMQOO_OUTPUT() {
          if ( (options & MQC.MQOO_OUTPUT) != 0) 
           return true;
       else
           return false;
    }

    /**
     * Set/unset the MQOO_SAVE_ALL_CONTEXT option.
     * 
     * @param bSet <code>True</code> to set the option.
     * 
     * @see com.ibm.mq.MQC#MQOO_SAVE_ALL_CONTEXT
     */
    public void setMQOO_SAVE_ALL_CONTEXT(boolean bSet) {
        if (bSet) {
            options |= MQC.MQOO_SAVE_ALL_CONTEXT;
        } else {
            options &= ~MQC.MQOO_SAVE_ALL_CONTEXT;
        }
    }
    
     public boolean getMQOO_SAVE_ALL_CONTEXT() {
         if ( (options & MQC.MQOO_SAVE_ALL_CONTEXT) != 0) 
           return true;
       else
           return false;
    }

    /**
     * Set/unset the MQOO_ALTERNATE_USER_AUTHORITY option.
     * 
     * @param bSet <code>True</code> to set the option.
     * 
     * @see com.ibm.mq.MQC#MQOO_ALTERNATE_USER_AUTHORITY
     */
    public void setMQOO_ALTERNATE_USER_AUTHORITY(boolean bSet) {
        if (bSet) {
            options |= MQC.MQOO_ALTERNATE_USER_AUTHORITY;
        } else {
            options &= ~MQC.MQOO_ALTERNATE_USER_AUTHORITY;
        }
    }
    
     public boolean getMQOO_ALTERNATE_USER_AUTHORITY() {
        if ( (options & MQC.MQOO_ALTERNATE_USER_AUTHORITY) != 0) 
           return true;
       else
           return false;
    }

    /**
     * Set/unset the MQOO_FAIL_IF_QUIESCING option.
     * 
     * @param bSet <code>True</code> to set the option.
     * 
     * @see com.ibm.mq.MQC#MQOO_FAIL_IF_QUIESCING
     */
    public void setMQOO_FAIL_IF_QUIESCING(boolean bSet) {
        if (bSet) {
            options |= MQC.MQOO_FAIL_IF_QUIESCING;
        } else {
            options &= ~MQC.MQOO_FAIL_IF_QUIESCING;
        }
    }
    
     public boolean getMQOO_FAIL_IF_QUIESCING() {
       if ( (options & MQC.MQOO_FAIL_IF_QUIESCING) != 0) 
           return true;
       else
           return false;
    }

    /**
     * Set/unset the MQOO_PASS_IDENTITY_CONTEXT option.
     * 
     * @param bSet <code>True</code> to set the option.
     * 
     * @see com.ibm.mq.MQC#MQOO_PASS_IDENTITY_CONTEXT
     */
    public void setMQOO_PASS_IDENTITY_CONTEXT(boolean bSet) {
        if (bSet) {
            options |= MQC.MQOO_PASS_IDENTITY_CONTEXT;
        } else {
            options &= ~MQC.MQOO_PASS_IDENTITY_CONTEXT;
        }
    }
    
    public boolean getMQOO_PASS_IDENTITY_CONTEXT() {
       if ( (options & MQC.MQOO_PASS_IDENTITY_CONTEXT) != 0) 
           return true;
       else
           return false;
    }

    /**
     * Set/unset the MQOO_PASS_ALL_CONTEXT option.
     * 
     * @param bSet <code>True</code> to set the option.
     * 
     * @see com.ibm.mq.MQC#MQOO_PASS_ALL_CONTEXT
     */
    public void setMQOO_PASS_ALL_CONTEXT(boolean bSet) {
        if (bSet) {
            options |= MQC.MQOO_PASS_ALL_CONTEXT;
        } else {
            options &= ~MQC.MQOO_PASS_ALL_CONTEXT;
        }
    }
    
    public boolean getMQOO_PASS_ALL_CONTEXT() {
       if ( (options & MQC.MQOO_PASS_ALL_CONTEXT) != 0) 
           return true;
       else
           return false;
    }

    /**
     * Set/unset the MQOO_SET_IDENTITY_CONTEXT option.
     * 
     * @param bSet <code>True</code> to set the option.
     * 
     * @see com.ibm.mq.MQC#MQOO_SET_IDENTITY_CONTEXT
     */
    public void setMQOO_SET_IDENTITY_CONTEXT(boolean bSet) {
        if (bSet) {
            options |= MQC.MQOO_SET_IDENTITY_CONTEXT;
        } else {
            options &= ~MQC.MQOO_SET_IDENTITY_CONTEXT;
        }
    }
    
    public boolean getMQOO_SET_IDENTITY_CONTEXT() {
         if ( (options & MQC.MQOO_SET_IDENTITY_CONTEXT) != 0) 
           return true;
       else
           return false;
    }

    /**
     * Set/unset the MQOO_SET_ALL_CONTEXT option.
     * 
     * @param bSet <code>True</code> to set the option.
     * 
     * @see com.ibm.mq.MQC#MQOO_SET_ALL_CONTEXT
     */
    public void setMQOO_SET_ALL_CONTEXT(boolean bSet) {
        if (bSet) {
            options |= MQC.MQOO_SET_ALL_CONTEXT;
        } else {
            options &= ~MQC.MQOO_SET_ALL_CONTEXT;
        }
    }
    
    public boolean  getMQOO_SET_ALL_CONTEXT() {
       if ( (options & MQC.MQOO_SET_ALL_CONTEXT) != 0) 
           return true;
       else
           return false;
    }

    /**
     * Set/unset the MQOO_INQUIRE option.
     * 
     * @param bSet <code>True</code> to set the option.
     * 
     * @see com.ibm.mq.MQC#MQOO_INQUIRE
     */
    public void setMQOO_INQUIRE(boolean bSet) {
        if (bSet) {
            options |= MQC.MQOO_INQUIRE;
        } else {
            options &= ~MQC.MQOO_INQUIRE;
        }
    }
    
    
     public boolean  getMQOO_INQUIRE() {
       if ( (options & MQC.MQOO_INQUIRE) != 0) 
           return true;
       else
           return false;
    }

    /**
     * Set/unset the MQOO_SET option.
     * 
     * @param bSet <code>True</code> to set the option.
     * 
     * @see com.ibm.mq.MQC#MQOO_SET
     */
    public void setMQOO_SET(boolean bSet) {
        if (bSet) {
            options |= MQC.MQOO_SET;
        } else {
            options &= ~MQC.MQOO_SET;
        }
    }
    
      public boolean getMQOO_SET() {
       if ( (options & MQC.MQOO_SET) != 0) 
           return true;
       else
           return false;
    }

    /**
     * Set/unset the MQOO_BIND_ON_OPEN option.
     * 
     * @param bSet <code>True</code> to set the option.
     * 
     * @see com.ibm.mq.MQC#MQOO_BIND_ON_OPEN
     */
    public void setMQOO_BIND_ON_OPEN(boolean bSet) {
        if (bSet) {
            options |= MQC.MQOO_BIND_ON_OPEN;
        } else {
            options &= ~MQC.MQOO_BIND_ON_OPEN;
        }
    }
    
     public boolean getMQOO_BIND_ON_OPEN() {
       if ( (options & MQC.MQOO_BIND_ON_OPEN) != 0) 
           return true;
       else
           return false;
    }

    /**
     * Set/unset the MQOO_BIND_NOT_FIXED option.
     * 
     * @param bSet <code>True</code> to set the option.
     * 
     * @see com.ibm.mq.MQC#MQOO_BIND_NOT_FIXED
     */
    public void setMQOO_BIND_NOT_FIXED(boolean bSet) {
        if (bSet) {
            options |= MQC.MQOO_BIND_NOT_FIXED;
        } else {
            options &= ~MQC.MQOO_BIND_NOT_FIXED;
        }
    }
    
     public boolean getMQOO_BIND_NOT_FIXED() {
       if ( (options & MQC.MQOO_BIND_NOT_FIXED) != 0) 
           return true;
       else
           return false;
    }

    /**
     * Set/unset the MQOO_BIND_AS_Q_DEF option.
     * 
     * @param bSet <code>True</code> to set the option.
     * 
     * @see com.ibm.mq.MQC#MQOO_BIND_AS_Q_DEF
     */
    public void setMQOO_BIND_AS_Q_DEF(boolean bSet) {
        if (bSet) {
            options |= MQC.MQOO_BIND_AS_Q_DEF;
        } else {
            options &= ~MQC.MQOO_BIND_AS_Q_DEF;
        }
    }
    
    public boolean getMQOO_BIND_AS_Q_DEF() {
       if ( (options & MQC.MQOO_BIND_AS_Q_DEF) != 0) 
           return true;
       else
           return false;
    }

    /**
     * Set/unset the MQOO_RESOLVE_NAMES option.
     * 
     * @param bSet <code>True</code> to set the option.
     * 
     * @see com.ibm.mq.MQC#MQOO_RESOLVE_NAMES
     */
    public void setMQOO_RESOLVE_NAMES(boolean bSet) {
        if (bSet) {
            options |= MQC.MQOO_RESOLVE_NAMES;
        } else {
            options &= ~MQC.MQOO_RESOLVE_NAMES;
        }
    }
    
    public boolean getMQOO_RESOLVE_NAMES() {
        if ( (options & MQC.MQOO_BIND_AS_Q_DEF) != 0) 
           return true;
       else
           return false;
    }

    public void parseOptions(String optionString) {
        StringTokenizer tokenizer = new StringTokenizer(optionString, ",");
        while (tokenizer.hasMoreTokens()) {
            String option = tokenizer.nextToken();
            for (OptionsVisitor visitor : optionsVisitors) {
                visitor.visit(option);
            }
        }
    }
    
    public String toOptionString() throws IOException {
        StringWriter swriter = new StringWriter();
        Writer writer = new BufferedWriter(swriter);
        for (OptionSerializerVisitor visitor : optionsSerializerVisitors) {
            visitor.visit(writer, this);
            writer.append(',');
        }
        writer.flush();
        writer.close();
        StringBuffer buffer = swriter.getBuffer();
        buffer.delete(buffer.length() - 1, buffer.length() - 1);
        return swriter.toString();
    }
    
    private class InputOpenOptionsVisitor implements OptionsVisitor {
        public void visit(String option) {
            option = option.trim();
            if ("READ_AS_DEFAULT".equalsIgnoreCase(option)) {
                QueueAccessOptions.this.setMQOO_INPUT_AS_Q_DEF(true);
                QueueAccessOptions.this.setMQOO_INPUT_EXCLUSIVE(false);
                QueueAccessOptions.this.setMQOO_INPUT_SHARED(false);
            } else if ("READ_EXCLUSIVE".equalsIgnoreCase(option)) {
                QueueAccessOptions.this.setMQOO_INPUT_AS_Q_DEF(false);
                QueueAccessOptions.this.setMQOO_INPUT_EXCLUSIVE(true);
                QueueAccessOptions.this.setMQOO_INPUT_SHARED(false);
            } else if ("READ_SHARED".equalsIgnoreCase(option)) {
                QueueAccessOptions.this.setMQOO_INPUT_AS_Q_DEF(false);
                QueueAccessOptions.this.setMQOO_INPUT_EXCLUSIVE(false);
                QueueAccessOptions.this.setMQOO_INPUT_SHARED(true);
            }
        }
    }
    
    private class BindOpenOptionsVisitor implements OptionsVisitor {
        public void visit(String option) {
            option = option.trim();
            if ("BIND_AS_DEFAULT".equalsIgnoreCase(option)) {
                QueueAccessOptions.this.setMQOO_BIND_AS_Q_DEF(true);
                QueueAccessOptions.this.setMQOO_BIND_NOT_FIXED(false);
                QueueAccessOptions.this.setMQOO_BIND_ON_OPEN(false);
            } else if ("BIND_NOT_FIXED".equalsIgnoreCase(option)) {
                QueueAccessOptions.this.setMQOO_BIND_AS_Q_DEF(false);
                QueueAccessOptions.this.setMQOO_BIND_NOT_FIXED(true);
                QueueAccessOptions.this.setMQOO_BIND_ON_OPEN(false);
            } else if ("BIND_ON_OPEN".equalsIgnoreCase(option)) {
                QueueAccessOptions.this.setMQOO_BIND_AS_Q_DEF(false);
                QueueAccessOptions.this.setMQOO_BIND_NOT_FIXED(false);
                QueueAccessOptions.this.setMQOO_BIND_ON_OPEN(true);
            }
        }
    }
    
    private class BindOpenOptionsSerializer implements OptionSerializerVisitor {
        public void visit(Writer writer, QueueAccessOptions options)
                throws IOException {
            if (options.getMQOO_BIND_AS_Q_DEF()) {
                writer.write("BIND_AS_DEFAULT");
            } else if (options.getMQOO_BIND_NOT_FIXED()) {
                writer.write("BIND_NOT_FIXED");
            } else if (options.getMQOO_BIND_ON_OPEN()) {
                writer.write("BIND_ON_OPEN");
            }
        }
    }
    
    private class InputOpenOptionsSerializer implements OptionSerializerVisitor {
        public void visit(Writer writer, QueueAccessOptions options)
                throws IOException {
            if (options.getMQOO_INPUT_AS_Q_DEF()) {
                writer.write("READ_AS_DEFAULT");
            } else if (options.getMQOO_INPUT_EXCLUSIVE()) {
                writer.write("READ_EXCLUSIVE");
            } else if (options.getMQOO_INPUT_SHARED()) {
                writer.write("READ_SHARED");
            }
        }
    }
    
    private interface OptionsVisitor {
        void visit(String option);
    }
    
    private interface OptionSerializerVisitor {
        void visit(Writer writer, QueueAccessOptions options)
                throws IOException;
    }
}
