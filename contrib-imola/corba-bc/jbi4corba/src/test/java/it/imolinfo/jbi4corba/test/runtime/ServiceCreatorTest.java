 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.test.runtime;

import it.imolinfo.jbi4corba.Logger;
import it.imolinfo.jbi4corba.LoggerFactory;
import it.imolinfo.jbi4corba.exception.Jbi4CorbaException;
import it.imolinfo.jbi4corba.jbi.JbiServiceDescriptor;
import it.imolinfo.jbi4corba.test.config.JarListHelper;
import it.imolinfo.jbi4corba.webservice.descriptor.ProviderServiceDescriptor;
import it.imolinfo.jbi4corba.webservice.generator.ClientCorbaClassesHolder;
import it.imolinfo.jbi4corba.webservice.generator.ProviderServiceClassesGenerator;
import it.imolinfo.jbi4corba.webservice.runtime.ProviderServiceCreator;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import javax.wsdl.WSDLException;

import junit.framework.TestCase;

import org.apache.cxf.service.Service;


/**
 * This class is used to test the code generation 'IDL to WSDL'.
 */
public class ServiceCreatorTest extends TestCase {

  /**
   * Logger
   */
  private static Logger log = LoggerFactory.getLogger(ServiceCreatorTest.class);

  /**
   * The directory where we place the IDLs used to generate the java code.
   */
  public static final String IDLDIR = "src/test/etc/idl";

  private List<String> jarFilesName = new ArrayList<String>();

  public ServiceCreatorTest(String arg0) {
    super(arg0);

    String repodir = System.getProperty("localRepository");
    log.debug("repodir=" + repodir);

    assertNotNull(repodir);
    assertFalse("".equals(repodir));
    jarFilesName = JarListHelper.extractJarList(repodir);
    
  }  
  
  /**
   * Testing for ... "Echo.idl"
   *
   * FIXME check the test
   */
//  public void testEchoServiceCreation() {
//    final String idlToTest = "Echo.idl";
//    final String targetDir = "target/testEchoServiceCreation";
//
//    log.debug(">>>>> testEchoServiceCreation - begin");
//    try {
//
//      testBody(idlToTest, targetDir, "testEchoServiceCreation");
//
//    } catch (Exception e) {
//      String m = "Error in ... testEchoServiceCreation:" + e.getMessage();
//      log.error(m, e);
//      e.printStackTrace();
//      fail(m);
//    }
//    log.debug("<<<<< testEchoServiceCreation - end");
//  }
//
//  /**
//   * Testing for ... "MultiModule.idl"
//   */
//  public void testMultiModuleServiceCreation() {
//    final String idlToTest = "MultiModule.idl";
//    final String targetDir = "target/testMultiModuleServiceCreation";
//
//    log.debug(">>>>> testMultiModuleServiceCreation - begin");
//    try {
//
//      testBody(idlToTest, targetDir, "testMultiModuleServiceCreation");
//
//    } catch (Exception e) {
//      String m = "Error in ... testMultiModuleServiceCreation:" + e.getMessage();
//      log.error(m, e);
//      e.printStackTrace();
//      fail(m);
//    }
//    log.debug("<<<<< testMultiModuleServiceCreation - end");
//  }
//
//
//    /**
//     * Testing for ... enum.
//     *
//     * CORBA has enumerators that are not explicitly tagged with values.
//     *
//     * The constraint is that any language mapping that permits two enumerators
//     * to be compared or defines successor or predecessor functions
//     * on enumerators must conform to the ordering of the enumerators
//     * as specified in the OMG IDL.
//     * Enum in IDL is mapped to �enumeration� of XML Schema with restriction
//     * placed on �string.�
//     *
//     * Example:
//     *
//     * // OMG IDL
//     * enum myEnum {A, B, C};
//     *
//     * This maps to:
//     *
//     * <!-- WSDL -->
//     * <xsd:simpleType name="myEnum">
//     *     <xsd:restriction base="xsd:string">
//     *         <xsd:enumeration value="A"/>
//     *         <xsd:enumeration value="B"/>
//     *         <xsd:enumeration value="C"/>
//     *         </xsd:restriction>
//     * </xsd:simpleType>
//     */
//    public void testEchoServiceCreationEnum() {
//        final String idlToTest = "EchoEnum.idl";
//        final String targetDir = "target/testEchoServiceCreationEnum";
//
//        log.debug(">>>>> testEchoServiceCreationEnum - begin");
//        try {
//
//            testBody(idlToTest, targetDir, "testEchoServiceCreationEnum");
//
//        } catch (Exception e) {
//            String m = "Error in ... testEchoServiceCreationEnum:"
//                    + e.getMessage();
//
//            log.error(m, e);
//            fail(m);
//        }
//        log.debug("<<<<< testEchoServiceCreationEnum - end");
//    }
//
//    /**
//    * Testing for ... array.
//    *
//    * In IDL the array data type has a specific size.
//    * If you want somethong like the "list" you must use the sequence.
//    *
//    * FROM: "Common Object Request Broker Architecture: Core Specification"
//    *
//    *        <array_declarator> ::= <identifier> <fixed_array_size>+
//    *
//    *      <fixed_array_size> ::= "[" <positive_int_const> "]"
//    */
//    public void testEchoServiceCreationArray() {
//        final String idlToTest = "EchoArray.idl";
//        final String targetDir = "target/testEchoServiceCreationArray";
//
//        log.debug(">>>>> testEchoServiceCreationArray - begin");
//        try {
//
//            testBody(idlToTest, targetDir, "testEchoServiceCreationArray");
//
//        } catch (Exception e) {
//            String m = "Error in ... testEchoServiceCreationArray:"
//                    + e.getMessage();
//
//            log.error(m, e);
//            e.printStackTrace();
//            fail(m);
//        }
//        log.debug("<<<<< testEchoServiceCreationArray - end");
//    }
//
//    /**
//    * Testing for ... array of struct.
//    */
//    public void testEchoServiceCreationArrayOfStruct() {
//        final String idlToTest = "EchoArrayOfStruct.idl";
//        final String targetDir = "target/testEchoServiceCreationArrayOfStruct";
//
//        log.debug(">>>>> testEchoServiceCreationArrayOfStruct - begin");
//        try {
//
//            testBody(idlToTest, targetDir, "testEchoServiceCreationArrayOfStruct");
//
//        } catch (Exception e) {
//            String m = "Error in ... testEchoServiceCreationArrayOfStruct:"
//                    + e.getMessage();
//
//            log.error(m, e);
//            e.printStackTrace();
//            fail(m);
//        }
//        log.debug("<<<<< testEchoServiceCreationArrayOfStruct - end");
//    }
//
//    /**
//    * Testing for ... exception.
//    */
//    public void testEchoServiceCreationException() {
//        final String idlToTest = "EchoException.idl";
//        final String targetDir = "target/testEchoServiceCreationException";
//
//        log.debug(">>>>> testEchoServiceCreationException - begin");
//        try {
//
//            testBody(idlToTest, targetDir, "testEchoServiceCreationException");
//
//        } catch (Exception e) {
//            String m = "Error in ... testEchoServiceCreationException:"
//                    + e.getMessage();
//
//            log.error(m, e);
//            e.printStackTrace();
//            fail(m);
//        }
//        log.debug("<<<<< testEchoServiceCreationException - end");
//    }
//
//    /**
//    * Testing for ... sequence.
//    */
//    public void testEchoServiceCreationSequence() {
//        final String idlToTest = "EchoSequence.idl";
//        final String targetDir = "target/testEchoServiceCreationSequence";
//
//        log.debug(">>>>> testEchoServiceCreationSequence - begin");
//        try {
//
//            testBody(idlToTest, targetDir, "testEchoServiceCreationSequence");
//
//        } catch (Exception e) {
//            String m = "Error in ... testEchoServiceCreationSequence:"
//                    + e.getMessage();
//
//            log.error(m, e);
//            e.printStackTrace();
//            fail(m);
//        }
//        log.debug("<<<<< testEchoServiceCreationSequence - end");
//    }
//
//    /**
//    * Testing for ... struct.
//    */
//    public void _testEchoServiceCreationStruct() {
//        final String idlToTest = "EchoStruct.idl";
//        final String targetDir = "target/testEchoServiceCreationStruct";
//
//        log.debug(">>>>> testEchoServiceCreationStruct - begin");
//        try {
//
//            testBody(idlToTest, targetDir, "testEchoServiceCreationStruct");
//
//        } catch (Exception e) {
//            String m = "Error in ... testEchoServiceCreationStruct:"
//                    + e.getMessage();
//
//            log.error(m, e);
//            e.printStackTrace();
//            fail(m);
//        }
//        log.debug("<<<<< testEchoServiceCreationStruct - end");
//    }
//
//    /**
//    * Testing for ... value type.
//    */
//    public void testEchoServiceCreationValueType() {
//        final String idlToTest = "EchoValueType.idl";
//        final String targetDir = "target/testEchoServiceCreationValueType";
//
//        log.debug(">>>>> testEchoServiceCreationValueType - begin");
//        try {
//
//            testBody(idlToTest, targetDir, "testEchoServiceCreationValueType");
//
//        } catch (Exception e) {
//            String m = "Error in ... testEchoServiceCreationValueType:"
//                    + e.getMessage();
//
//            log.error(m, e);
//            e.printStackTrace();
//            fail(m);
//        }
//        log.debug("<<<<< testEchoServiceCreationValueType - end");
//    }
//
//
//
//    // ================================
//    //                       VALUE TYPE
//    // ================================
//
//    /*
//        FROM : "Common Object Request Broker Architecture: Core Specification"
//
//        3.9 Value Declaration
//
//        There are several kinds of value type declarations:
//            regular value types,
//            boxed value types,
//            abstract value types,
//            and forward declarations.
//
//        A value declaration satisfies the following syntax:
//
//            <value> ::= ( <value_dcl> | <value_abs_dcl>
//                                      | <value_box_dcl>
//                                      | <value_forward_dcl>)
//    */
//
//    /**
//        FROM : "Common Object Request Broker Architecture: Core Specification"
//
//        3.9.1 Regular Value Type
//
//        A regular value type satisfies the following syntax:
//
//            <value_dcl> ::= <value_header> "{" < value_element>* "}"
//
//            <value_header> ::= ["custom" ] "valuetype" <identifier>
//                               [ <value_inheritance_spec> ]
//
//            <value_element> ::= <export> | < state_member>
//                                         | <init_dcl>
//
//        3.9.1.1 Value Header
//
//            The value header consists of two elements:
//
//                1. The value type�s name and optional modifier specifying
//                   whether the value type uses custom marshaling.
//
//                2. An optional value inheritance specification.
//
//        3.9.1.2 Value Element
//
//            A value can contain all the elements that an interface can
//            as well as the definition of state members,
//            and initializers for that state.
//
//        [...]
//
//        3.9.1.6 Value Type Example
//
//            interface Tree {
//                void print()
//            };
//
//            valuetype WeightedBinaryTree {
//                // state definition
//                private unsigned long weight;
//                private WeightedBinaryTree left;
//                private WeightedBinaryTree right;
//
//                // initializer
//                factory init(in unsigned long w);
//
//                // local operations
//                WeightSeq pre_order();
//                WeightSeq post_order();
//            };
//
//            valuetype WTree: WeightedBinaryTree supports Tree {};
//
//        ====
//
//        FROM : "CORBA to WSDL/SOAP Interworking Specification - Version 1.2"
//
//        4.1.7.10 ValueType
//
//            Value types in CORBA are mapped the same way as structures in WSDL.
//
//            The value type WSDL will always be generated,
//            but it is up to individual implementations to decide whether
//            they will support the processing of these types.
//
//            For example, DII/DSI bridges will be unable to process value types.
//            An implementation may reject a message containing a value type
//            parameter, raising a NO_IMPLEMENT exception with a standard minor
//            code, as follows:
//
//            Minor Code 9  : Valuetypes not supported by CORBA-WSDL/SOAP implementation.
//            Minor Code 10 : Valuetype sharing not supported by CORBA-WSDL/SOAP implementation.
//
//            Value type inheritance is supported by expanding the inheritance
//            graph to include all inherited public and private state members
//            in the representation for the inheriting value type.
//            The state members from the inherited value type appear in the mapped
//            struct before the members added in the inheriting value type
//            definition.
//
//        4.1.7.11 valuetype
//
//            Basic valuetypes are mapped to structs, as noted above.
//            Both Public and Private fields are mapped.
//            For example, consider the following:
//
//            // IDL
//            valuetype sampleX {
//                public short a;
//                private long b;
//            }
//
//            This maps to:
//
//            // WSDL
//            <xsd:complexType name="sampleX">
//                <xsd:sequence>
//                    <xsd:element name="a" type="xsd:short" maxOccurs="1" minOccurs="1"/>
//                    <xsd:element name="b" type="xsd:int" maxOccurs="1" minOccurs="1"/>
//                </xsd:sequence>
//
//                <xsd:attribute name="id" type="xsd:ID" use="optional" />
//                <!-- id must be present if value type instance is shared (i.e.,
//                referenced as a struct or value type member using xml IDREF)-->
//            </xsd:complexType>
//    */
//    public void testEchoServiceCreationRegularValueType() {
//
//        final String idlToTest = "EchoRegularValueType.idl";
//        final String targetDir = "target/testEchoServiceCreationRegularValueType";
//
//        log.debug(">>>>> testEchoServiceCreationRegularValueType - begin");
//        try {
//
//            testBody(idlToTest, targetDir, "testEchoServiceCreationRegularValueType");
//
//            log.warn("The WSDL generated has minOccurs=0");
//
//        } catch (Exception e) {
//            String m = "Error in ... testEchoServiceCreationRegularValueType:"
//                    + e.getMessage();
//
//            log.error(m, e);
//            e.printStackTrace();
//            fail(m);
//        }
//        log.debug("<<<<< testEchoServiceCreationRegularValueType - end");
//    }
//
//
//    /**
//        FROM : "Common Object Request Broker Architecture: Core Specification"
//
//        3.9.2 Boxed Value Type
//
//            <value_box_dcl> ::= "valuetype" <identifier> <type_spec>
//
//            It is often convenient to define a value type with
//                - no inheritance
//                - or operations
//                - and with a single state member.
//
//            A shorthand IDL notation is used to simplify the use of value
//            types for this kind of simple containment,
//            referred to as a "value box."
//
//            Since a value box of a valuetype adds no additional properties
//            to a valuetype, it is an error to box valuetypes.
//
//            Value box is particularly useful for strings and sequences.
//            Basically one does not have to create what is in effect
//            an additional namespace that will contain only one name.
//
//            An example is the following IDL:
//
//                module Example {
//                    interface Foo {
//                        // ... anything
//                    };
//
//                    valuetype FooSeq sequence<Foo>;
//
//                    interface Bar {
//                        void doIt (in FooSeq seq1);
//                    };
//                };
//
//                The above IDL provides similar functionality to writing
//                the following IDL. However the type identities (repository ID�s)
//                would be different.
//
//                module Example {
//                    interface Foo {
//                        //...  anything
//                    };
//
//                    valuetype FooSeq {
//                        public sequence<Foo> data;
//                    };
//
//                    interface Bar {
//                        void doIt (in FooSeq seq);
//                    };
//                };
//
//                The former is easier to manipulate after it is mapped to
//                a concrete programming language.
//
//                Any IDL type may be used to declare a value box
//                except for a valuetype.
//                The declaration of a boxed value type does not open a new scope.
//                Thus a construction such as:
//
//                    valuetype FooSeq sequence <FooSeq>;
//
//                is not legal IDL.
//
//                The identifier being declared as a boxed value type cannot
//                be used subsequent to its initial use and prior to
//                the completion of the boxed value declaration.
//        ====
//
//        FROM : "CORBA to WSDL/SOAP Interworking Specification - Version 1.2"
//
//        4.1.7.12 valuebox
//
//            Valueboxes for primitive types are mapped to a struct
//            with a single member (called "value"), whose data type is mapped
//            according to the primitive mapping table given in Section 4.1.6,
//            "Primitive Types," on page 6.
//
//            Valueboxes for sequences, arrays, and structs
//            are mapped similarly to a struct with a single member
//            (called "value"), whose data type will be the XML schema type
//            defined by the appropriate mapping for that sequence,
//            array, or struct.
//    */
//    public void testEchoServiceCreationValueBoxedType() {
//        final String idlToTest = "EchoValueBoxTypes.idl";
//        final String targetDir
//            = "target/testEchoServiceCreationValueBoxedType";
//
//        log.debug(">>>>> testEchoServiceCreationValueBoxedType - begin");
//        try {
//
//            testBody(idlToTest,
//                    targetDir,
//                    "testEchoServiceCreationValueBoxedType");
//
//        } catch (Exception e) {
//            String m = "Error in ... testEchoServiceCreationValueBoxedType:"
//                    + e.getMessage();
//
//            log.error(m, e);
//            e.printStackTrace();
//            fail(m);
//        }
//        log.debug("<<<<< testEchoServiceCreationValueBoxedType - end");
//    }
//
//
//    /**
//        FROM : "Common Object Request Broker Architecture: Core Specification"
//
//        3.9.3 Abstract Value Type
//
//            <value_abs_dcl> ::= "abstract" "valuetype" <identifier>
//                                [ <value_inheritance_spec> ]
//                                "{" <export>* "}"
//
//            Value types may also be abstract.
//            They are called abstract because an abstract value type
//                - may not be instantiated.
//                - No <state_member> or <initializers> may be specified.
//
//            However, local operations may be specified.
//            Essentially they are a bundle of operation signatures with
//            a purely local implementation.
//
//            Note that a concrete value type with an empty state is not
//            an abstract value type.
//        */
//        public void _testEchoServiceCreationAbstractValueType() {
//            final String idlToTest = "EchoAbstractValueType.idl";
//            final String targetDir
//                = "target/testEchoServiceCreationAbstractValueType";
//
//            log.debug(">>>>> testEchoServiceCreationAbstractValueType - begin");
//            try {
//
//                testBody(idlToTest,
//                        targetDir,
//                        "testEchoServiceCreationAbstractValueType");
//
//            } catch (Exception e) {
//                String m = "Error in ... testEchoServiceCreationAbstractValueType:"
//                        + e.getMessage();
//
//                log.error(m, e);
//                e.printStackTrace();
//                fail(m);
//            }
//            log.debug("<<<<< testEchoServiceCreationAbstractValueType - end");
//        }

    // ================================
    //      IDL with all the data types
    // ================================


  
  /**
   * This is a test where we use a complex idl.
   */
  public void testEchoServiceCreationComplex() {
    final String idlToTest = "EchoComplex.idl";
    final String targetDir = "target/testEchoServiceCreationComplex";

    log.debug(">>>>> testEchoServiceCreationComplex - begin");
    try {

      testBody(idlToTest, targetDir, "testEchoServiceCreationComplex");

    } catch (Exception e) {
    	e.printStackTrace();
      String m = "Error in ... testEchoServiceCreationComplex:"
              + e.getMessage();

      log.error(m, e);
      fail(m);
    }
    log.debug("<<<<< testEchoServiceCreationComplex - end");
  }


    /**
    * XXX javadoc
    */
    public void testEchoSC() {
        final String idlToTest = "generator/webservice/test/jbi4corba/imolinfo/it/EchoSCServicePortType.idl";
        final String targetDir = "target/testEchoSC";

        log.debug(">>>>> testEchoSC - begin");
        try {

            testBody("sc", idlToTest, targetDir, "EchoSC");

    } catch (Exception e) {
        String m = "Error in ... testEchoSC()";
            log.error(m, e);
            fail(m + ". cause:" + e.getMessage());
    }
    log.debug("<<<<< testEchoSC - end");
    }

    /**
     * XXX javadoc
     */
    public void testEchoSCFULL() {
        //final String idlToTest = "generator/webservice/test/jbi4corba/imolinfo/it/EchoSCServicePortTypeCorbaInterface.idl";
        final String idlToTest = "EchoSCServicePortTypeCorbaInterface.idl";
        final String targetDir = "target/testEchoSCFULL";

        log.debug(">>>>> testEchoSCFULL - begin");
        try {

            testBody("sc-full", idlToTest, targetDir, "EchoSC");

        } catch (Exception e) {
            String m = "Error in ... testEchoSCFULL()";
            log.error(m, e);
            fail(m + ". cause:" + e.getMessage());
        }
        log.debug("<<<<< testEchoSCFULL - end");
    }

    /**
     * @see http://195.223.140.228:9090/browse/CRB-106
     */
    public void testServiceCreatorEchoCrb106() {
      final String idlToTest = "EchoCrb106.idl";
      final String targetDir = "target/testServiceCreatorEchoCrb106";

      log.debug(">>>>> testServiceCreatorEchoCrb106 - begin");
      try {

        testBodyMoreInterfaces(idlToTest, targetDir, "EchoCrb106");

      } catch (Exception e) {
        String m = "Error in ... testServiceCreatorEchoCrb106()";
        log.error(m, e);
        fail(m + ". cause:" + e.getMessage());
      }
      log.debug("<<<<< testServiceCreatorEchoCrb106 - end");
    }

    /**
     * @see http://195.223.140.228:9090/browse/CRB-106
     */
    public void testServiceCreatorEchoCrb106consumer() {
      final String idlToTest = "EchoCrb106ConsumerPortTypeCorbaInterface.idl";
      final String targetDir = "target/testServiceCreatorEchoCrb106consumer";

      log.debug(">>>>> testServiceCreatorEchoCrb106 - begin");
      try {

        testBody("crb106consumer", idlToTest, targetDir, "EchoCrb106Consumer");

      } catch (Exception e) {
        String m = "Error in ... testServiceCreatorEchoCrb106consumer()";
        log.error(m, e);
        fail(m + ". cause:" + e.getMessage());
      }
      log.debug("<<<<< testServiceCreatorEchoCrb106consumer - end");
    }

    /**
    * This is a test where we use a complex idl.
    */
    public void testCrb108WrongWsdlParameter() {
        final String idlToTest = "Crb108WrongWsdlParameter.idl";
        final String targetDir = "target/testCrb108WrongWsdlParameter";

        log.debug(">>>>> testCrb108WrongWsdlParameter - begin");
        try {

            testBody(idlToTest, targetDir, "testCrb108WrongWsdlParameter");

        } catch (Exception e) {
            String m = "Error in ... testCrb108WrongWsdlParameter:"
                    + e.getMessage();

            log.error(m, e);
            e.printStackTrace();
            fail(m);
        }
        log.debug("<<<<< testCrb108WrongWsdlParameter - end");
    }

    /**
     * @see http://195.223.140.228:9090/browse/CRB-41
     */
    public void testServiceCreatorOneway() {
      final String idlToTest = "Async.idl";
      final String targetDir = "target/testServiceCreatorOneway";

      log.debug(">>>>> testServiceCreatorOneway - begin");
      try {

        testBodyMoreInterfaces(idlToTest, targetDir, "EchoOneway");

      } catch (Exception e) {
        String m = "Error in ... testServiceCreatorOneway()";
        log.error(m, e);
        fail(m + ". cause:" + e.getMessage());
      }
      log.debug("<<<<< testServiceCreatorOneway - end");
    }
    
    /**
     * This is a test where we use a complex idl.
     */
    public void testEchoSequence() {
      final String idlToTest = "EchoSequence.idl";
      final String targetDir = "target/testEchoSequence";

      log.debug(">>>>> testEchoSequence - begin");
      try {

        testBody(idlToTest, targetDir, "testEchoSequence");

      } catch (Exception e) {
        String m = "Error in ... testEchoSequence:"
                + e.getMessage();

        log.error(m, e);
        fail(m);
      }
      log.debug("<<<<< testEchoSequence - end");
    }    
    
    /**
     * This is a test where we use a complex idl.
     */
    public void testEchoSimpleInOut() {
      final String idlToTest = "EchoInOut.idl";
      final String targetDir = "target/testEchoSimpleInOut";

      log.debug(">>>>> testEchoSimpleInOut - begin");
      try {

        testBody(idlToTest, targetDir, "testEchoSimpleInOut");

      } catch (Exception e) {
        String m = "Error in ... testEchoSimpleInOut:"
                + e.getMessage();

        log.error(m, e);
        fail(m);
      }
      log.debug("<<<<< testEchoSimpleInOut - end");
    }    
    
    /**
     * This is a test where we use a complex idl.
     */
    public void testEchoComplexInOut2() {
      final String idlToTest = "EchoComplexInOut2.idl";
      final String targetDir = "target/testEchoComplexInOut2";

      log.debug(">>>>> testEchoSimpleInOut - begin");
      try {

        testBody(idlToTest, targetDir, "testEchoComplexInOut2");

      } catch (Exception e) {
        String m = "Error in ... testEchoComplexInOut2:"
                + e.getMessage();

        log.error(m, e);
        fail(m);
      }
      log.debug("<<<<< testEchoComplexInOut2 - end");
    }       

    // ================================
    //                  Utility Methods
    // ================================

    /**
     * IDL -> Corba (java/classes) -> WSDL
     *
     * @param idlToTest 
     * @param targetDir 
     * @param serv 
     *
     * @throws java.io.IOException 
     * @throws Jbi4CorbaException 
     * @throws WSDLException 
     */
    private void testBodyMoreInterfaces(
      String idlToTest, String targetDir, String serv)
      throws IOException, WSDLException, Jbi4CorbaException {

      log.debug(">>>>> testBody - begin");

      JbiServiceDescriptor jbiServiceDescriptor = new JbiServiceDescriptor();

      jbiServiceDescriptor.setIdlFileNameDirectory(IDLDIR);
      jbiServiceDescriptor.setIdlFileName(idlToTest);

      ProviderServiceClassesGenerator generator
        = new ProviderServiceClassesGenerator();

      List<ClientCorbaClassesHolder> classes
        = generator.generateProviderServiceClasses(
          jbiServiceDescriptor, targetDir, jarFilesName, null);//new param:jars

      log.debug("classes: " + classes);

      assertNotNull("classes must be not null", classes);
      assertFalse("classes.isEmpty() must be false", classes.isEmpty());

      log.debug("List<ClientCorbaClassesHolder>.size=" + classes.size());

      for (ClientCorbaClassesHolder corbaServiceClasses : classes) {
        log.debug("========== createService for " + corbaServiceClasses);
        createService(corbaServiceClasses, serv,null);
        log.debug("==========");
      }

      log.debug(">>>>> testBody - end");
    }

    private void createService(ClientCorbaClassesHolder corbaServiceClasses,
      String serv,java.lang.String serviceNameSpace) throws IOException, WSDLException, Jbi4CorbaException {

      ProviderServiceDescriptor serviceDescriptor
        = new ProviderServiceDescriptor();

      serviceDescriptor.setServiceInterface(
        corbaServiceClasses.getOperationsClass());
      serviceDescriptor.setCorbaHelperClass(
        corbaServiceClasses.getHelperClass());

      serviceDescriptor.setServiceName(serv);
      if (serviceNameSpace==null){
      serviceDescriptor.setServiceNameSpace(
        serviceDescriptor.getServiceInterface().getPackage().getName());
      }
      else {
          serviceDescriptor.setServiceNameSpace(serviceNameSpace);
      }

      log.debug("service namespace:" + serviceDescriptor.getServiceNameSpace());

      ProviderServiceCreator serviceCreator = new ProviderServiceCreator();
      Service service = serviceCreator.createService(serviceDescriptor);

      log.info("service created: " + service);
    }

    /**
     * IDL -> Corba (java/classes) -> WSDL
     *
     * @param idlToTest 
     * @param targetDir 
     * @param serv 
     *
     * @throws java.io.IOException 
     * @throws Jbi4CorbaException 
     * @throws WSDLException 
     */
    private void testBody(String idlToTest, String targetDir, String serv)
        throws IOException, WSDLException, Jbi4CorbaException {

        log.debug(">>>>> testBody - begin");

        JbiServiceDescriptor jbiServiceDescriptor
            = new JbiServiceDescriptor();

        jbiServiceDescriptor.setIdlFileNameDirectory(IDLDIR);
        jbiServiceDescriptor.setIdlFileName(idlToTest);

        //jbiServiceDescriptor.setServiceNameSpace("urn:unitTestNameSpace");
        jbiServiceDescriptor.setServiceNameSpace("http://echocomplex.generator.webservice.test.jbi4corba.imolinfo.it/");
        
        
        ProviderServiceClassesGenerator serviceGenerator
            = new ProviderServiceClassesGenerator();

        //List<String> jars = null;
        List<ClientCorbaClassesHolder> classes
            = serviceGenerator.generateProviderServiceClasses(
                    jbiServiceDescriptor,
                    targetDir,
                    jarFilesName, null); // new params

        log.debug("classes: " + classes);

        assertNotNull("classes must be not null", classes);
        assertFalse("classes.isEmpty() must be false", classes.isEmpty());

        // we know there is just one class
        ClientCorbaClassesHolder corbaServiceClasses = classes.get(0);
        
        createService(corbaServiceClasses, serv,jbiServiceDescriptor.getServiceNameSpace());
        log.debug(">>>>> testBody - end");
    }

    /**
    * IDL -> Corba (java/classes) -> WSDL
     * @throws Jbi4CorbaException 
     * @throws WSDLException 
    */
    private void testBody(String subIdlDir, String idlToTest, String targetDir, String serv)
        throws IOException, WSDLException, Jbi4CorbaException {

        log.debug(">>>>> testBody - begin");

        JbiServiceDescriptor jbiServiceDescriptor
            = new JbiServiceDescriptor();

        jbiServiceDescriptor.setIdlFileNameDirectory(IDLDIR + File.separator + subIdlDir);
        jbiServiceDescriptor.setIdlFileName(idlToTest);

        ProviderServiceClassesGenerator serviceGenerator
            = new ProviderServiceClassesGenerator();

        //List<String> jars = null;
        List<ClientCorbaClassesHolder> classes
            = serviceGenerator.generateProviderServiceClasses(
                    jbiServiceDescriptor,
                    targetDir,
                    jarFilesName,  // new params
                    null);

        log.debug("classes: " + classes);

        assertNotNull("classes must be not null", classes);
        assertFalse("classes.isEmpty() must be false", classes.isEmpty());

        // we know there is just on class
        ClientCorbaClassesHolder corbaServiceClasses = classes.get(0);

        createService(corbaServiceClasses, serv,null);
        log.debug(">>>>> testBody - end");
    }

}
