<?xml version="1.0" encoding="UTF-8"?>
<definitions name="OutputToFile" targetNamespace="http://j2ee.netbeans.org/wsdl/OutputToFile"
    xmlns="http://schemas.xmlsoap.org/wsdl/"
    xmlns:wsdl="http://schemas.xmlsoap.org/wsdl/"
    xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:tns="http://j2ee.netbeans.org/wsdl/OutputToFile" xmlns:plnk="http://docs.oasis-open.org/wsbpel/2.0/plnktype" xmlns:file="http://schemas.sun.com/jbi/wsdl-extensions/file/">
    <types/>
    <message name="OutputToFileOperationRequest">
        <part name="FileData" type="xsd:string"/>
    </message>
    <portType name="OutputToFilePortType">
        <operation name="OutputToFileOperation">
            <input name="input1" message="tns:OutputToFileOperationRequest"/>
        </operation>
    </portType>
    <binding name="OutputToFileBinding" type="tns:OutputToFilePortType">
        <file:binding/>
        <operation name="OutputToFileOperation">
            <file:operation/>
            <input name="input1">
                <file:message use="literal" fileName="{@oneway_bpel1_output_file@}" addEOL="true" multipleRecordsPerFile="true"/>
            </input>
        </operation>
    </binding>
    <service name="OutputToFileService">
        <port name="OutputToFilePort" binding="tns:OutputToFileBinding">
            <file:address fileDirectory="{@jbi_qe_tmp_dir@}/{@tonga_test_name@}" lockName="filebc.lck" workArea="filebc_tmp" seqName="filebc.seq"/>
        </port>
    </service>
    <plnk:partnerLinkType name="OutputToFile">
        <!-- A partner link type is automatically generated when a new port type is added. Partner link types are used by BPEL processes. 
In a BPEL process, a partner link represents the interaction between the BPEL process and a partner service. Each partner link is associated with a partner link type.
A partner link type characterizes the conversational relationship between two services. The partner link type can have one or two roles.-->
        <plnk:role name="OutputToFilePortTypeRole" portType="tns:OutputToFilePortType"/>
    </plnk:partnerLinkType>
</definitions>
