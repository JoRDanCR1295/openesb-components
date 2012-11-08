/**
 * Redistribution and use of this software and associated documentation
 * ("Software"), with or without modification, are permitted provided
 * that the following conditions are met:
 *
 * 1. Redistributions of source code must retain copyright
 *    statements and notices.  Redistributions must also contain a
 *    copy of this document.
 *
 * 2. Redistributions in binary form must reproduce the
 *    above copyright notice, this list of conditions and the
 *    following disclaimer in the documentation and/or other
 *    materials provided with the distribution.
 *
 * 3. The name "Exolab" must not be used to endorse or promote
 *    products derived from this Software without prior written
 *    permission of Intalio, Inc.  For written permission,
 *    please contact info@exolab.org.
 *
 * 4. Products derived from this Software may not be called "Exolab"
 *    nor may "Exolab" appear in their names without prior written
 *    permission of Intalio, Inc. Exolab is a registered
 *    trademark of Intalio, Inc.
 *
 * 5. Due credit should be given to the Exolab Project
 *    (http://www.exolab.org/).
 *
 * THIS SOFTWARE IS PROVIDED BY INTALIO, INC. AND CONTRIBUTORS
 * ``AS IS'' AND ANY EXPRESSED OR IMPLIED WARRANTIES, INCLUDING, BUT
 * NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
 * FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL
 * INTALIO, INC. OR ITS CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * Copyright 1999-2001 (C) Intalio, Inc. All Rights Reserved.
 *
 * 
 */

package org.exolab.castor.xml.schema;

//-- we should change this to SchemaValidationException
//-- and localize the package
import javax.swing.event.EventListenerList;

import org.exolab.castor.xml.ValidationException;


/**
 * The base class for all XML Schema stuctures.
 *
 * @author <a href="mailto:kvisco@intalio.com">Keith Visco</a>
 * @version  
**/
public abstract class Structure implements java.io.Serializable {

    public static final short ANYTYPE             =  0;
    public static final short ANNOTATION          =  1;
    public static final short APPINFO             =  2;
    public static final short ATTRIBUTE           =  3;
    public static final short ATTRIBUTE_GROUP     =  4;
    public static final short COMPLEX_CONTENT     =  5;
    public static final short COMPLEX_TYPE        =  6;
    public static final short DOCUMENTATION       =  7;
    public static final short ELEMENT             =  8;
    public static final short FACET               =  9;
    public static final short GROUP               = 10;
    public static final short IDENTITY_FIELD      = 11;
    public static final short IDENTITY_SELECTOR   = 12;
    public static final short KEY                 = 13;
    public static final short KEYREF              = 14;
    public static final short LIST                = 15;
    public static final short MODELGROUP          = 16;
    public static final short MODELGROUP_REF      = 17;
    public static final short REDEFINE            = 18;
    public static final short SCHEMA              = 19;
    public static final short SIMPLE_CONTENT      = 20;
    public static final short SIMPLE_TYPE         = 21;
    public static final short UNION               = 22;
    public static final short UNIQUE              = 23;
    public static final short WILDCARD            = 24;

    //-- should be removed eventually
    public static final short UNKNOWN         = -1;

    private Structure _parent = null;
    
    /** A list of event listeners for this component. */
    protected EventListenerList listenerList = new EventListenerList();

    
    /**
     * Creates a new XML Schema Structure
    **/
    protected Structure() {
        super();
    } //-- Structure

    /**
     * Calls validate() to determine if this Schema Definition
     * is valid.
     *
     * @return true if this Schema definition is valid, otherwise false.
    **/
    public boolean isValid() {
        try {
            validate();
        }
        catch(ValidationException ex) {
            return false;
        }
        return true;
    } //-- isValid

    /**
     * Returns the type of this Schema Structure.
     *
     * @return the type of this Schema Structure.
    **/
    public abstract short getStructureType();

    /**
     * Checks the validity of this Schema defintion.
     *
     * @exception ValidationException when this Schema definition
     * is invalid.
    **/
    public abstract void validate()
        throws ValidationException;
    
    /**
     * Sets the parent for this Structure
     *
     * @param parent the parent Structure for this Structure
    **/
    public void setParent(Structure parent) {
    	this._parent = parent;
    }
    
    /**
     * Gets the parent for this Structure
     * this value may be null if no parent has been set.
     * @return parent the parent Structure for this Structure
    **/
    public Structure getParent() {
    	return this._parent;
    }
    
    public void addSchemaEventListener(SchemaEventListener l) {
    	listenerList.add(SchemaEventListener.class, l);
    }
    
    public void removeSchemaEventListener(SchemaEventListener l) {
    	listenerList.remove(SchemaEventListener.class, l);
    }
    
    
    protected void fireAttributeAdded(String attrName, Object value) {
    	AttributeEvent attributeEvent = null;
        // Guaranteed to return a non-null array
        Object[] listeners = listenerList.getListenerList();
        // Process the listeners last to first, notifying
        // those that are interested in this event
        for (int i = listeners.length-2; i>=0; i-=2) {
            if (listeners[i]==SchemaEventListener.class) {
                if(attributeEvent == null) {
                	attributeEvent = 
	                	new AttributeEvent(this, 
	                						  attrName, 
											  null, 
											  value);
                }
                ((SchemaEventListener)listeners[i+1]).attributeAdded(attributeEvent);
            }
        }
        
        //propogate event to parent
        if(attributeEvent != null 
           && this.getParent() != null) {
        		this.getParent().fireAttributeAdded(attributeEvent);
        }
     }
    
    protected void fireAttributeRemoved(String attrName, Object value) {
    	AttributeEvent attributeEvent = null;
        // Guaranteed to return a non-null array
        Object[] listeners = listenerList.getListenerList();
        // Process the listeners last to first, notifying
        // those that are interested in this event
        for (int i = listeners.length-2; i>=0; i-=2) {
            if (listeners[i]==SchemaEventListener.class) {
                if(attributeEvent == null) {
                	attributeEvent = 
	                	new AttributeEvent(this, 
	                						  attrName, 
											  null, 
											  value);
                }
                ((SchemaEventListener)listeners[i+1]).attributeRemoved(attributeEvent);
            }
        }
        
        //propogate event to parent
        if(attributeEvent != null 
           && this.getParent() != null) {
        		this.getParent().fireAttributeRemoved(attributeEvent);
        }
     }
    
    protected void fireAttributeModified(String attrName, Object oldValue, Object newValue) {
    	AttributeEvent attributeEvent = null;
        // Guaranteed to return a non-null array
        Object[] listeners = listenerList.getListenerList();
        // Process the listeners last to first, notifying
        // those that are interested in this event
        for (int i = listeners.length-2; i>=0; i-=2) {
            if (listeners[i]==SchemaEventListener.class) {
                if(attributeEvent == null) {
                	attributeEvent = 
	                	new AttributeEvent(this, 
	                						  attrName, 
	                						  oldValue, 
											  newValue);
                }
                ((SchemaEventListener)listeners[i+1]).attributeModified(attributeEvent);
            }
        }
        
        //propogate event to parent
        if(attributeEvent != null 
           && this.getParent() != null) {
        		this.getParent().fireAttributeModified(attributeEvent);
        }
     }
    
    protected void fireAttributeEvent(String attrName, Object oldValue, Object newValue) {
    	if(oldValue == null) {
    		fireAttributeAdded(attrName, newValue);
    	} else if(newValue == null) {
    		fireAttributeRemoved(attrName, oldValue);
    	} else {
    		fireAttributeModified(attrName, oldValue, newValue);
    	}
     }
    
    protected void fireStructureAdded(Structure contained) {
    	StructureEvent structureEvent = null;
        // Guaranteed to return a non-null array
        Object[] listeners = listenerList.getListenerList();
        // Process the listeners last to first, notifying
        // those that are interested in this event
        for (int i = listeners.length-2; i>=0; i-=2) {
            if (listeners[i]==SchemaEventListener.class) {
                if(structureEvent == null) {
                	structureEvent = 
	                	new StructureEvent(this, contained);
                }
                ((SchemaEventListener)listeners[i+1]).structureAdded(structureEvent);
            }
        }
        
        //propogate event to parent
        if(structureEvent != null 
           && this.getParent() != null ) {
        		this.getParent().fireStructureAdded(structureEvent);
        }
     }
    
    protected void fireStructureRemoved(Structure contained) {
    	StructureEvent structureEvent = null;
        // Guaranteed to return a non-null array
        Object[] listeners = listenerList.getListenerList();
        // Process the listeners last to first, notifying
        // those that are interested in this event
        for (int i = listeners.length-2; i>=0; i-=2) {
            if (listeners[i]==SchemaEventListener.class) {
                if(structureEvent == null) {
                	structureEvent = 
	                	new StructureEvent(this, contained);
                }
                ((SchemaEventListener)listeners[i+1]).structureRemoved(structureEvent);
            }
        }
        
        //propogate event to Schema
        if(structureEvent != null 
           && this.getParent() != null) {
        		this.getParent().fireStructureRemoved(structureEvent);
        }
     }
    
    protected void fireStructureAdded(StructureEvent evt) {
    	//  Guaranteed to return a non-null array
    	    Object[] listeners = listenerList.getListenerList();
    	    // Process the listeners last to first, notifying
    	    // those that are interested in this event
    	    for (int i = listeners.length-2; i>=0; i-=2) {
    	        if (listeners[i]==SchemaEventListener.class) {
    	        	((SchemaEventListener)listeners[i+1]).structureAdded(evt);
    	        }
    	    }
    	    
    	    //propogate event to parent
            if(evt != null
            	&& this.getParent() != null) {
            		this.getParent().fireStructureAdded(evt);
            }
     }
        
    protected void fireStructureRemoved(StructureEvent evt) {
    		//  Guaranteed to return a non-null array
    	    Object[] listeners = listenerList.getListenerList();
    	    // Process the listeners last to first, notifying
    	    // those that are interested in this event
    	    for (int i = listeners.length-2; i>=0; i-=2) {
    	        if (listeners[i]==SchemaEventListener.class) {
    	        	((SchemaEventListener)listeners[i+1]).structureRemoved(evt);
    	        }
    	    }
    	    
    	    //propogate event to parent
            if(evt != null
            	&& this.getParent() != null) {
            		this.getParent().fireStructureRemoved(evt);
            }
       }
   
    protected void fireAttributeAdded(AttributeEvent evt) {
		//  Guaranteed to return a non-null array
	    Object[] listeners = listenerList.getListenerList();
	    // Process the listeners last to first, notifying
	    // those that are interested in this event
	    for (int i = listeners.length-2; i>=0; i-=2) {
	        if (listeners[i]==SchemaEventListener.class) {
	        	((SchemaEventListener)listeners[i+1]).attributeAdded(evt);
	        }
	    }
	    
	    //propogate event to parent
        if(evt != null 
           && this.getParent() != null) {
        		((Structure) this.getParent()).fireAttributeAdded(evt);
        }
   }
     
    protected void fireAttributeRemoved(AttributeEvent evt) {
		//  Guaranteed to return a non-null array
	    Object[] listeners = listenerList.getListenerList();
	    // Process the listeners last to first, notifying
	    // those that are interested in this event
	    for (int i = listeners.length-2; i>=0; i-=2) {
	        if (listeners[i]==SchemaEventListener.class) {
	        	((SchemaEventListener)listeners[i+1]).attributeRemoved(evt);
	        }
	    }
	    
	    //propogate event to parent
        if(evt != null 
           && this.getParent() != null) {
        		((Structure) this.getParent()).fireAttributeRemoved(evt);
        }
        
   }
   
    protected void fireAttributeModified(AttributeEvent evt) {
		//  Guaranteed to return a non-null array
	    Object[] listeners = listenerList.getListenerList();
	    // Process the listeners last to first, notifying
	    // those that are interested in this event
	    for (int i = listeners.length-2; i>=0; i-=2) {
	        if (listeners[i]==SchemaEventListener.class) {
	        	((SchemaEventListener)listeners[i+1]).attributeModified(evt);
	        }
	    }
	    
	    //propogate event to parent
	    if(evt != null 
           && this.getParent() != null) {
        		((Structure) this.getParent()).fireAttributeModified(evt);
        }
   }
} //-- Structure
