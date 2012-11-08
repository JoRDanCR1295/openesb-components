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
 * Copyright 1999-2002 (C) Intalio, Inc. All Rights Reserved.
 *
 * 
 */

package org.exolab.castor.xml.schema.reader;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;
import java.io.FileReader;
import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.InputStreamReader;

import java.io.Reader;
import java.io.IOException;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import org.exolab.castor.net.URIResolver;
import org.exolab.castor.util.Configuration;
import org.exolab.castor.util.LocalConfiguration;
import org.exolab.castor.util.NestedIOException;

import org.exolab.castor.xml.schema.Schema;
import org.exolab.castor.xml.XMLException;

import org.xml.sax.InputSource;
import org.xml.sax.EntityResolver;
import org.xml.sax.Parser;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;


import com.sun.org.apache.xml.internal.serialize.Serializer;

/**
 * A class for reading XML Schemas
 *
 * @author <a href="mailto:kvisco@intalio.com">Keith Visco</a>
 * @version  
**/
public class SchemaReader {

    /**
     * The Castor Configuration 
     */
    private Configuration _config = null;

    /**
     * XML Parser instance
     */
    private Parser      _parser   = null;
    
    /**
     * SAX InputSource to Schema
     */
    private InputSource _source   = null;
    
    /**
     * SAX EntityResolver
     */
    private EntityResolver _resolver = null;


     /**
     * The resolver to be used for resolving href
     */
    private URIResolver _uriResolver;

    private Schema      _schema   = null;

    private boolean     _validate = true;
    
    /**
     * Modified by hlin 02-12-2003 to keep defaultSchemaLocation
     */
    private String mDefaultFileLocation = null;
    
    /** SeeBeyond extension: list of property change listeners to notifiy */
    private Set propertyChangeListeners = null;
 
    /**
     * Creates a new SchemaReader
     */
    private SchemaReader()
        throws IOException
    {
        //-- get default parser from Configuration
        
        _config = LocalConfiguration.getInstance();
        
        Parser parser = null;
        
	parser = _config.getParser();

        if (parser == null) {
            throw new IOException("fatal error: unable to create SAX parser.");
        }

        _parser = parser;
    } //-- SchemaReader

    /**
     * Modified by hlin 02-12-2003 to keep defaultSchemaLocation
     * @param fileName the absolute directory of the xsd file
     * we need to set the defaultLocation to the directory where
     * the xsd is found so that we don't run into problems parsing any
     * relative external files referenced by the dtd.
     * Creates a new SchemaReader
     */
    public SchemaReader(File xsdFile)
        throws IOException
    {
        this();
        
        mDefaultFileLocation = xsdFile.getParentFile().getAbsolutePath();
        //System.out.println("...............mDefaultFileLocation:"+ mDefaultFileLocation);
        _source = new InputSource(new BufferedReader(new FileReader(xsdFile)));
    } //-- SchemaReader
    
    /**
     * Modified by hlin 02-12-2003 to keep defaultSchemaLocation
     * @param fileName the absolute directory of the xsd file
     * we need to set the defaultLocation to the directory where
     * the xsd is found so that we don't run into problems parsing any
     * relative external files referenced by the dtd.
     * Creates a new SchemaReader
     */
    public SchemaReader(File xsdFile, String encoding)
        throws IOException
    {
        this();
        
        mDefaultFileLocation = xsdFile.getParentFile().getAbsolutePath();
        //System.out.println("...............mDefaultFileLocation:"+ mDefaultFileLocation);
        if (encoding.compareTo("default") == 0) { //default by locale
        _source = new InputSource(new BufferedReader(new FileReader(xsdFile)));
		} else {
	        FileInputStream fis = new FileInputStream(xsdFile);
	        //_source = new InputSource(new BufferedReader(new FileReader(xsdFile)));
	        _source = new InputSource(new BufferedReader(new InputStreamReader(fis, encoding)));
	    }
    } //-- SchemaReader

    /**
     * Creates a new SchemaReader for the given InputSource
     *
     * @param source the InputSource to read the Schema from.
    **/
    public SchemaReader(InputSource source)
        throws IOException
    {
        this();

        if (source == null)
            throw new IllegalArgumentException("InputSource cannot be null");

        _source = source;

    } //-- SchemaReader

    /**
     * Creates a new SchemaReader for the given Reader
     *
     * @param reader the Reader to read the Schema from.
     * @param filename for reporting errors.
    **/
    public SchemaReader(Reader reader, String filename)
        throws IOException
    {
        this();

        if (reader == null) {
            String err = "The argument 'reader' must not be null.";
            throw new IllegalArgumentException(err);
        }

        _source = new InputSource(reader);
        if (filename == null) filename = reader.toString();
        _source.setPublicId(filename);

    } //-- SchemaReader

    /**
     * Creates a new SchemaReader for the given URL
     *
     * @param url the URL string
    **/
    public SchemaReader(String url)
        throws IOException
    {
        this();
        if (url == null) {
            String err = "The argument 'url' must not be null.";
            throw new IllegalArgumentException(err);
        }
        _source = new InputSource(url);

    } //-- SchemaReader

    /**
     * Reads the Schema from the source and returns the Schema
     * object model.
     *
     * <BR />
     * <B>Note:</B> Subsequent calls to this method will simply
     * return a cached copy of the Schema object. To read a new
     * Schema object, create a new Reader.
     *
     * @return the new Schema created from the source of this SchemaReader
    **/
    public Schema read() throws IOException
    {
        if (_schema != null) return _schema;

        SchemaUnmarshaller schemaUnmarshaller = null;

        try {
            SchemaUnmarshallerState state = new SchemaUnmarshallerState();
            state.setConfiguration(_config);
            
            schemaUnmarshaller = new SchemaUnmarshaller(state);    
            if (_uriResolver != null)
                schemaUnmarshaller.setURIResolver(_uriResolver);
            
            // SeeBeyond extension: chain the SchemaUnmarshaller back to this
            schemaUnmarshaller.setSchemaReader(this);
            
            /**
             * Modified by hlin 02-12-2003 to keep defaultSchemaLocation
             */
            if (schemaUnmarshaller.getURIResolver() != null) {
                schemaUnmarshaller.getURIResolver().setDefaultFileLocation(mDefaultFileLocation);
            }
                
            Sax2ComponentReader handler
                = new Sax2ComponentReader(schemaUnmarshaller);
            _parser.setDocumentHandler(handler);
            _parser.setErrorHandler(handler);
            if (_resolver != null)
                _parser.setEntityResolver(_resolver);
            _parser.parse(_source);
        }
        catch(XMLException ex) {
            handleException(ex);
        }
        catch(org.xml.sax.SAXException sx) {
            handleException(sx);
        }

        _schema = schemaUnmarshaller.getSchema();

        if (_validate) {
            try {
                _schema.validate();
            }
            catch(org.exolab.castor.xml.ValidationException vx) {
                vx.printStackTrace();
                throw new NestedIOException(vx);
            }
        }

        return _schema;

    } //-- read

    /**
     * Sets whether or not post-read validation should
     * occur. By default, validation is enabled. Note
     * that certain read validation cannot be disabled.
     *
     * @param validate a boolean that when true will force
     * a call to Schema#validate after the schema is read.
    **/
    public void setValidation(boolean validate) {
        _validate = validate;
    } //-- setValidation


    /**
     * Sets the EntityResolver used to resolve SYSTEM Identifier.
     * If the entity resolver is null, the default one will be used.
     *
     * @param resolver the EntityResolver to use.
     */
    public void setEntityResolver(EntityResolver resolver) {
        _resolver = resolver;
    }

    /**
     * Sets the URIResolver used to resolve hrefs.
     * If the entity resolver is null, the default one will be used.
     *
     * @param resolver the URIResolver to use.
     */
    public void setURIResolver(URIResolver uriresolver) {
        _uriResolver = uriresolver;
    }

    private void handleException(XMLException xx)
        throws IOException
    {
        xx.printStackTrace();
        Exception except = xx.getException();
        if (except == null) {
            except = xx;
        }
        else if (except instanceof SAXException) {
            handleException((SAXException)except);
        }
        else if (except instanceof XMLException) {
            handleException((XMLException)except);
        }

        throw new NestedIOException(except);
    } //-- handleException

    private void handleException(SAXException sx)
        throws IOException
    {
        sx.printStackTrace();
        Exception except = sx.getException();
        if (except == null) {
            except = sx;
        }
        else if (except instanceof SAXParseException) {
            SAXParseException spe = (SAXParseException)except;
            String filename = spe.getSystemId();
            if (filename == null) filename = spe.getPublicId();
            if (filename == null) filename = "<filename unavailable>";

            String err = spe.getMessage();

            err += "; " + filename + " [ line: " + spe.getLineNumber();
            err += ", column: " + spe.getColumnNumber() + ']';
            throw new NestedIOException(err, except);
        }
        else if (except instanceof XMLException) {
            handleException((XMLException)except);
        }

        throw new NestedIOException(except);

    } //-- handleException
    
    /** SeeBeyond extension.  Add a property change listener to this object.
     * @param   listener    Listener interested in changes.
     */
    public void addPropertyChangeListener(PropertyChangeListener listener) {
        if (null == propertyChangeListeners) {
            propertyChangeListeners = new HashSet(1);
        }
        propertyChangeListeners.add(listener);
    }
    
    /** SeeBeyond extension.  Remove a property change listener of this object.
     * @param   listener    Listener to remove.
     */
    public void removePropertyChangeListener(PropertyChangeListener listener) {
        if (propertyChangeListeners != null) {
            propertyChangeListeners.remove(listener);
        }
    }
    
    /** SeeBeyond extension.  Remove all property change listeners of this object.
     */
    public void removeAllPropertyChangeListeners() {
        if (propertyChangeListeners != null) {
            propertyChangeListeners.clear();
        }
    }

    /** SeeBeyond extension.  Notify all listeners of change.
     * @param   event   Change event.
     */
    public void firePropertyChange(PropertyChangeEvent event) {
        if (propertyChangeListeners != null) {
            Iterator iter = propertyChangeListeners.iterator();
            while (iter.hasNext()) {
                ((PropertyChangeListener) iter.next()).propertyChange(event);
            }
        }
    }
    
    /**
     * For testing purposes.
     * @param args command line parameters 
     */
    public static void main(String[] args) {
        if (args.length < 2) {
            System.out.println("Usage: SchemaReader -[f|u] filename|url");
            return;
        }
        try {
            String argType = args[0];
            SchemaReader schemaReader = null;
            if ("-f".equals(argType)) {
                File xsdFile = new File(args[1]);
                schemaReader = new SchemaReader(xsdFile);
            } else {
                String url = args[1];
                schemaReader = new SchemaReader(url);
            }
            Schema schema = schemaReader.read();
            System.out.println("SUCCESS!");
        } catch (StackOverflowError soe) {
            System.err.println("StackOverflowError");
        } catch (Throwable e) {
            e.printStackTrace();
        }
    }
    
} //-- SchemaReader
