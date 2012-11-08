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

//-- imported classes and packages
import org.exolab.castor.net.*;
import org.exolab.castor.xml.*;
import org.exolab.castor.xml.schema.*;
import org.xml.sax.*;

import java.util.HashMap;
import java.util.Map;
import java.util.Enumeration;

/**
 * <p>Title: </p>
 * <p>Description: </p>
 * <p>Copyright: Copyright (c) 2004</p>
 * <p>Company: SeeBeyond Technology Corporation</p>
 * @author Jun Xu
 * @version 1.0
 */
public class RedefineUnmarshaller extends ComponentReader{
    /**
     * The current branch depth
     **/
    private int depth = 0;

    /**
     * The current ComponentReader
     **/
    private ComponentReader unmarshaller;

    private Schema _schema = null;

    private boolean foundAnnotation = false;
    private boolean foundSimpleType = false;
    private boolean foundComplexType = false;
    private boolean foundModelGroup = false;
    private boolean foundAttrGroup = false;

    private static Map _nameMap = new HashMap();

    public static final String REDEFINEPREFIX = "__redefine__";

    public RedefineUnmarshaller
        (Schema schema, AttributeSet atts, Resolver resolver,
         URIResolver uriResolver,
         Locator locator, SchemaUnmarshallerState state) throws XMLException {

        super();
        setResolver(resolver);
        setURIResolver(uriResolver);
        this._schema = schema;
        URILocation uri = null;
        //-- Get schemaLocation
        String include = atts.getValue("schemaLocation");
        if (include == null)
            throw new SchemaException(
                "'schemaLocation' attribute missing on 'redefine'");

        // SeeBeyond extension.
        // Convert any schemaLocations in the MSWindows format to the proper
        // URI format.  This is to support customers who refuse to change
        // their XSD files to use a URI compliant format.  Refer to Bug1416.
        include = winToURIFormat(include);

        if (include.indexOf("\\") != -1) {
            String err = include +
                " is not a valid URI as defined by IETF RFC 2396.";
            err += "The URI must not contain '\\'.";
            throw new SchemaException(err);
        }

        try {
            String documentBase = locator.getSystemId();
            if (documentBase != null) {
                if (!documentBase.endsWith("/"))
                    documentBase = documentBase.substring(0,
                        documentBase.lastIndexOf("/") + 1);
            }
            uri = getURIResolver().resolve(include, documentBase);
        }
        catch (URIException ure) {
            throw new XMLException(ure);
        }

        if (uri != null)
            include = uri.toString();

        include = include.replaceAll("%20", " "); // Added by Jun Xu, SeeBeyond Technology Corp.

        //-- Has this schema location been processed?
        if (state.processed(include)) {
            return;
        }
        state.markAsProcessed(include, schema);

        //just keep track of the schemaLocation
        schema.addInclude(include);

        Parser parser = null;
        try {
            parser = state.getConfiguration().getParser();
        }
        catch (RuntimeException rte) {}
        if (parser == null) {
            throw new SchemaException(
                "Error failed to create parser for redefine");
        }
        else {
            SchemaUnmarshaller schemaUnmarshaller = new SchemaUnmarshaller(true,
                state, getURIResolver());
            schemaUnmarshaller.setSchema(schema);
            Sax2ComponentReader handler = new Sax2ComponentReader(
                schemaUnmarshaller);
            parser.setDocumentHandler(handler);
            parser.setErrorHandler(handler);
        }

        try {
            InputSource source = new InputSource(uri.getReader());
            source.setSystemId(uri.getAbsoluteURI());
            parser.parse(source);
        }
        catch (java.io.IOException ioe) {
            throw new SchemaException("Error reading redefine file '" + include +
                                      "'");
        }
        catch (org.xml.sax.SAXException sx) {
            throw new SchemaException(sx);
        }
    }

    /**
     * reset the redefine name map, so all revisions in the redefine name will
     * start from zero.
     * This method usually should be called once from SchemaReader.read().
     */
    public static void resetRedefineNameMap() {
        _nameMap = new HashMap();
    }

    /**
     * get a redefine name based on the original name
     * E.g., if the original name is "foo", and latest redefine name has been used
     * is "__redefine__foo_V0", then the returned redefine name will be
     * "__redefine__foo_V1"
     *
     * @param originalName String The original name of the type being redefined
     * @return String the redefine name
     */
    public static String getRedefineName(String originalName) {
        String name;
        Integer serial;
        if ( (serial = (Integer) _nameMap.get(originalName)) == null) {
            name = REDEFINEPREFIX + originalName + "_V0";
            _nameMap.put(originalName, new Integer(1));
            return name;
        }
        name = REDEFINEPREFIX + originalName + "_V" + serial.intValue();
        _nameMap.put(originalName, new Integer(serial.intValue() + 1));
        return name;
    }

    /**
     * Signals the start of an element with the given name.
     *
     * @param name the NCName of the element. It is an error
     * if the name is a QName (ie. contains a prefix).
     * @param namespace the namespace of the element. This may be null.
     * Note: A null namespace is not the same as the default namespace unless
     * the default namespace is also null.
     * @param atts the AttributeSet containing the attributes associated
     * with the element.
     * @param nsDecls the namespace declarations being declared for this
     * element. This may be null.
     **/
    public void startElement(String name, String namespace, AttributeSet atts,
                             Namespaces nsDecls) throws XMLException {

        //-- Do delagation if necessary
        if (unmarshaller != null) {
            unmarshaller.startElement(name, namespace, atts, nsDecls);
            ++depth;
            return;
        }

        if (SchemaNames.ANNOTATION.equals(name)) {
            if (foundSimpleType || foundComplexType || foundModelGroup ||
                foundAttrGroup)
                error("An annotation may only appear as the first child " +
                      "of a redefine definition.");

            if (foundAnnotation)
                error("Only one (1) 'annotation' is allowed as a child of " +
                      "redefine definitions.");

            foundAnnotation = true;
            unmarshaller = new AnnotationUnmarshaller(atts);
        }
        else if (SchemaNames.COMPLEX_TYPE.equals(name)) {
            foundComplexType = true;
            unmarshaller
                = new ComplexTypeUnmarshaller(_schema, atts, getResolver());
        }
        else if (SchemaNames.SIMPLE_TYPE.equals(name)) {
            foundSimpleType = true;
            unmarshaller = new SimpleTypeUnmarshaller(_schema, atts);
        }
        else if (SchemaNames.GROUP.equals(name)) {
            foundModelGroup = true;
            unmarshaller = new ModelGroupUnmarshaller(_schema, atts,
                getResolver());
        }
        else if (SchemaNames.ATTRIBUTE_GROUP.equals(name)) {
            foundAttrGroup = true;
            unmarshaller = new AttributeGroupUnmarshaller(_schema, atts);
        }
        else {
            illegalElement(name);
        }

        unmarshaller.setResolver(getResolver());
        unmarshaller.setDocumentLocator(getDocumentLocator());

    } //-- startElement

    /**
     * Signals to end of the element with the given name.
     *
     * @param name the NCName of the element. It is an error
     * if the name is a QName (ie. contains a prefix).
     * @param namespace the namespace of the element.
     **/
    public void endElement(String name, String namespace) throws XMLException {

        //-- Do delagation if necessary
        if ( (unmarshaller != null) && (depth > 0)) {
            unmarshaller.endElement(name, namespace);
            --depth;
            return;
        }

        //-- check for name mismatches
        if (unmarshaller != null) {
            if (!name.equals(unmarshaller.elementName())) {
                String err = "missing end element for ";
                err += unmarshaller.elementName();
                throw new SchemaException(err);
            }
        }

        //-- call finish for any necessary cleanup
        unmarshaller.finish();

        if (SchemaNames.ANNOTATION.equals(name)) {
            //throw it away.  Only care about the components that have
            //been redefined
        } else if (SchemaNames.COMPLEX_TYPE.equals(name)) {

            ComplexType complexType
                = ( (ComplexTypeUnmarshaller) unmarshaller).getComplexType();

            ComplexType originalCType;
            if ( (originalCType = _schema.getComplexType(complexType.getName())) == null) {
                String err =
                    "missing corresponding complex type for redefining '";
                err += complexType.getName() + "'";
                throw new SchemaException(err);
            }

            // Clone the complex type that has been redefined
            ComplexType redefinedType = (ComplexType) originalCType.clone();
            // Set the redefined complex type to a different name with revision number
            redefinedType.setName(getRedefineName(redefinedType.getName()));
            String originalId = originalCType.getId();
            // Change the Id to the Id of the new complex type, so we can preserve
            // the original Id for the redefining complex type.  This maybe not important
            // just to be cautious
            redefinedType.setId(complexType.getId());
            originalCType.copyFrom(complexType);
            originalCType.setId(originalId);
            originalCType.setBase(redefinedType.getName());
            _schema.addComplexType(redefinedType);

        } else if (SchemaNames.SIMPLE_TYPE.equals(name)) {
            SimpleType simpleType
                = ( (SimpleTypeUnmarshaller) unmarshaller).getSimpleType();

            SimpleType originalSType;
            if ( (originalSType = _schema.getSimpleType(simpleType.getName())) == null) {
                String err =
                    "missing corresponding simple type for redefining '";
                err += simpleType.getName() + "'";
                throw new SchemaException(err);
            }

            // Clone the simple type that has been redefined
            SimpleType redefinedType = (SimpleType) originalSType.clone();
            // Set the redefined complex type to a different name with revision number
            redefinedType.setName(getRedefineName(redefinedType.getName()));
            String originalId = originalSType.getId();
            redefinedType.setId(simpleType.getId());
            simpleType.setBaseType(redefinedType);
            originalSType.copyFrom( (SimpleType) simpleType);
            _schema.addSimpleType(redefinedType);
        } else if (SchemaNames.GROUP.equals(name)) {

            ModelGroup modelGroup
                = ( (ModelGroupUnmarshaller) unmarshaller).getGroup();

            ModelGroup originalModelGroup;
            if ( (originalModelGroup = _schema.getModelGroup(modelGroup.getName(true))) == null) {
                String err =
                    "missing corresponding model group for redefining '";
                err += modelGroup.getName(true) + "'";
                throw new SchemaException(err);
            }

            ModelGroup cloneOfOriginal = (ModelGroup) originalModelGroup.clone();
            cloneOfOriginal.setName(getRedefineName(originalModelGroup.getName(true)));
            _schema.addModelGroup(cloneOfOriginal);
            int count = replaceModelGroupRef(modelGroup, originalModelGroup,
                                          cloneOfOriginal);
            if (count > 1) {
                String err =
                    "only one reference is allowed to the redefined group for redefining '";
                err += modelGroup.getName(true) + "'";
                throw new SchemaException(err);
            }
            if (count == 1) {
                //The redefined group contains exactly one reference to the ooriginal group, so
                //it's considered as superset redefinition.
                originalModelGroup.copyFrom(modelGroup);
            } else if (count == 0) {
                //The redefined group does not contain the reference to the original group, so
                //it's considered as subset redefininition and we need to validate if the
                //redefined group is a subset of the original one
                validateModelGroupSubset (originalModelGroup, modelGroup);
                originalModelGroup.copyFrom(modelGroup);
                _schema.removeGroup(cloneOfOriginal);
            } else {
                // not possible
                String err =
                    "invalid self-reference count ( < 0 ) for group: '";
                err += modelGroup.getName(true) + "'";
                throw new SchemaException(err);
            }
        } else if (SchemaNames.ATTRIBUTE_GROUP.equals(name)) {
            AttributeGroup attrGroup
                = ( (AttributeGroupUnmarshaller) unmarshaller).
                getAttributeGroup();

            AttributeGroup originalAttrGroup;
            if ( (originalAttrGroup = _schema.getAttributeGroup(attrGroup.
                getName())) == null) {
                String err =
                    "missing corresponding attribute group for redefining '";
                err += attrGroup.getName() + "'";
                throw new SchemaException(err);
            }

            validateAttrGroupRedefine(originalAttrGroup, attrGroup);
            AttributeGroupDecl ag = new AttributeGroupDecl(_schema);
            Enumeration e = attrGroup.getAttributes();
            while (e.hasMoreElements()) {
                ag.addAttribute( (AttributeDecl) e.nextElement());
            }
            ag.setAnyAttribute(attrGroup.getAnyAttribute());
            ag.setName(attrGroup.getName());
            ag.setId(attrGroup.getId());
            //Since all the references to an attribute group are resolved by
            //the attribute group name, we can safely replace the original
            //attribute group with redefine one, which has the same name.
            _schema.removeAttributeGroup(originalAttrGroup);
            _schema.addAttributeGroup(ag);
        }

        unmarshaller = null;

    } //-- endElement

    public static boolean validateOccurrenceRestriction (Particle restricted,
        Particle restricting) {

        if ( (restricted.getMinOccurs() > restricting.getMinOccurs())
            || (restricted.getMaxOccurs() >= 1 && restricting.getMaxOccurs() < 1)
            || (restricted.getMaxOccurs() >= 1 && restricting.getMaxOccurs() >= 1 &&
                restricted.getMaxOccurs() < restricting.getMaxOccurs())) {

           return false;
       }

       return true;
    }

    public static void validateModelGroupSubset (ModelGroup original, ModelGroup subset)
        throws  XMLException {

        String err =
            "The particles in model group '" + subset.getName(true) + "' do not form a "
            + "legal subset for the particles in the original model group '"
            + original.getName(true) + "'.";

        if (original.getOrder() != null && subset.getOrder() == null
            || original.getOrder() == null && subset.getOrder() != null) {

            throw new SchemaException(err);
        }
        if (original.getOrder() != null && subset.getOrder() != null
            && original.getOrder().getType() != subset.getOrder().getType()) {

            throw new SchemaException(err);
        }

        int countOriginal = original.getParticleCount();
        int countSubset = subset.getParticleCount();
        int j = 0;
        for (int i = 0; i < countOriginal; i++) {
            Particle pcOriginal = original.getParticle(i);
            Particle pcSubset;
            if ((pcSubset = subset.getParticle(j)) == null) {
                if (pcOriginal.getMinOccurs() > 0) {
                    //If in the original model group the particle is not optional, then it
                    //must appear in the subset model group
                    throw new SchemaException(err);
                }
                continue;
            }
            if (pcOriginal instanceof ModelGroup) {
                if (pcSubset instanceof ModelGroup) {
                    String nameOriginal = ((ModelGroup) pcOriginal).getName();
                    String nameSubset = ((ModelGroup) pcSubset).getName();
                    if ((nameOriginal == null && nameSubset == null)
                        || (nameOriginal != null && nameSubset != null
                        && nameOriginal.equals(nameSubset))) {

                        if (!validateOccurrenceRestriction (pcOriginal, pcSubset)) {
                            throw new SchemaException(err +
                                "subset model group contains illegal" +
                                " occurrence restrictions");
                        }

                        validateModelGroupSubset ((ModelGroup) pcOriginal,
                                                  (ModelGroup) pcSubset);
                        //matched
                        j++;
                    } else {
                        if (pcOriginal.getMinOccurs() > 0) {
                            throw new SchemaException(err
                                + " missing particle in subset model group.");
                        }
                    }
                } else {
                    if (pcOriginal.getMinOccurs() > 0) {
                        throw new SchemaException(err
                                                  +" missing particle in subset model group.");
                    }
                }
            } else if (pcOriginal instanceof ElementDecl) {
                if (pcSubset instanceof ElementDecl) {
                    if (!((ElementDecl)pcOriginal).getName().equals(((ElementDecl)pcSubset).getName())) {
                        if (pcOriginal.getMinOccurs() > 0) {
                            throw new SchemaException(err
                                + " Element '"
                                + ( (ElementDecl) pcOriginal).getName() + "'"
                                + " is required, but '"
                                + ( (ElementDecl) pcSubset).getName() + "' was found.");
                        } else {
                            continue;
                        }
                    }
                    if (!validateOccurrenceRestriction (pcOriginal, pcSubset)) {
                        throw new SchemaException(err +
                            "subset model group contains illegal" +
                            " occurrence restrictions");
                    }

                    //matched
                    j++;
                } else {
                    if (pcOriginal.getMinOccurs() > 0) {
                        throw new SchemaException(err
                                                  +" missing particle in subset model group.");
                    }
                }
            } else if (pcOriginal instanceof Wildcard) {
                if (pcSubset instanceof Wildcard) {
                    //Should check namespace here, but let's consider it as matched for now.
                    if (!validateOccurrenceRestriction (pcOriginal, pcSubset)) {
                        throw new SchemaException(err +
                            "subset model group contains illegal" +
                            " occurrence restrictions");
                    }
                    j++;
                } else {
                    //how can we validate here?
                    if (pcOriginal.getMaxOccurs() < 1) {
                        //the occurrence for the wildcard is unbounded
                        //consume all remain paticles in subset
                        j = countSubset;
                        continue;
                    } else {
                        int consumed = 0;
                        while (consumed < pcOriginal.getMaxOccurs()) {
                            if (consumed + pcSubset.getMaxOccurs() < pcOriginal.getMaxOccurs()) {
                                j++;
                                consumed += pcSubset.getMaxOccurs();
                                if ((pcSubset = subset.getParticle(j)) == null) {
                                    break;
                                }
                            } else if (consumed + pcSubset.getMaxOccurs() == pcOriginal.getMaxOccurs()) {
                                j++;
                                break;
                            } else {
                                // consider this as unmatched situation since the number of
                                // the occurrences of the wildcard does not exactly enclose
                                // the sum of the occurrences of the restricting particles
                                throw new SchemaException(err +
                                    " the wildcard is not legally restricted.");
                            }
                        }
                    }
                }
            } else {
                //Unknown particle instance is encountered.  No validation, let it be.
                throw new SchemaException("Unknown particle instance found in model"
                                          + " group '" + original.getName() + "'");
            }
        }
    }

    public static void validateAttrGroupRedefine (AttributeGroup original, AttributeGroup redefine)
        throws  XMLException {

        //Need to validate either a superset or subset
        boolean isSuperSet = false;
        if (original.getAnyAttribute() != null) {
            //the original group has anyAttribute, so it does not matter what's defined
            //in the redefined group
            return;
        }
        Enumeration eRedefine;
        Enumeration eOriginal;
        if (original.getAnyAttribute() == null && redefine.getAnyAttribute() != null) {
            isSuperSet = true;
        } else {
            eRedefine = redefine.getAttributes();
            while (eRedefine.hasMoreElements()) {
                AttributeDecl attr = (AttributeDecl) eRedefine.nextElement();
                if (original.getAttribute(attr.getName()) == null) {
                    isSuperSet = true;
                    break;
                }
            }
        }
        if (isSuperSet) {
            // Check if all the attributes defined in the original group
            // are included in the redefined group
            eOriginal = original.getAttributes();
            while (eOriginal.hasMoreElements()) {
                AttributeDecl attr = (AttributeDecl) eOriginal.nextElement();
                if (redefine.getAttribute(attr.getName()) == null) {
                    throw new SchemaException(
                        "Illegal redefine for attribute group: '"
                        + original.getName() + "'");
                }
            }
        } else {
            // Check if all the mandatory attributes on the original group have been
            // included in the redefined group
            eOriginal = original.getAttributes();
            while (eOriginal.hasMoreElements()) {
                AttributeDecl attr = (AttributeDecl) eOriginal.nextElement();
                if (attr.isRequired()) {
                    if (redefine.getAttribute(attr.getName()) == null) {
                        throw new SchemaException(
                            "Illegal restricting redefine for attribute group: '"
                            + original.getName() + "'");
                    }
                }
            }
        }
    }

    private int replaceModelGroupRef(Group groupToSearch, ModelGroup toBeReplaced,
                                  ModelGroup Replacement) throws
        SchemaException {
        int count = groupToSearch.replaceGroupRef(toBeReplaced, Replacement);
        Enumeration e = groupToSearch.enumerate();
        while (e.hasMoreElements()) {
            Object o = e.nextElement();
            if (o instanceof Group) {
                count = count +
                    replaceModelGroupRef( (Group) o, toBeReplaced, Replacement);
            }
        }
        return count;
    }

    public void characters(char[] ch, int start, int length) throws
        XMLException {
        //-- Do delagation if necessary
        if (unmarshaller != null) {
            unmarshaller.characters(ch, start, length);
        }
    } //-- characters

    /**
     * Sets the name of the element that this UnknownUnmarshaller handles
     * @param name the name of the element that this unmarshaller handles
     **/
    public String elementName() {
        return SchemaNames.REDEFINE;
    } //-- elementName

    /**
     * Returns the Object created by this ComponentReader
     * @return the Object created by this ComponentReader
     **/
    public Object getObject() {
        return null;
    } //-- getObject
}
