/**
 * ﻿Copyright (C) 2007 - 2014 52°North Initiative for Geospatial Open Source
 * Software GmbH
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 as published
 * by the Free Software Foundation.
 *
 * If the program is linked with libraries which are licensed under one of
 * the following licenses, the combination of the program with the linked
 * library is not considered a "derivative work" of the program:
 *
 *       • Apache License, version 2.0
 *       • Apache Software License, version 1.0
 *       • GNU Lesser General Public License, version 3
 *       • Mozilla Public License, versions 1.0, 1.1 and 2.0
 *       • Common Development and Distribution License (CDDL), version 1.0
 *
 * Therefore the distribution of the program linked with libraries licensed
 * under the aforementioned licenses, is permitted by the copyright holders
 * if the distribution is compliant with both the GNU General Public
 * License version 2 and the aforementioned licenses.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
 * Public License for more details.
 */
package org.n52.wps.server.algorithm;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Properties;

import org.n52.lod.Configuration;
import org.n52.lod.ProgressListener;
import org.n52.lod.csw.CSWLoDEnabler;
import org.n52.wps.algorithm.annotation.Algorithm;
import org.n52.wps.algorithm.annotation.ComplexDataOutput;
import org.n52.wps.algorithm.annotation.Execute;
import org.n52.wps.algorithm.annotation.LiteralDataInput;
import org.n52.wps.commons.WPSConfig;
import org.n52.wps.io.data.GenericFileData;
import org.n52.wps.io.data.binding.complex.GenericFileDataBinding;
import org.n52.wps.io.data.binding.literal.LiteralStringBinding;
import org.n52.wps.server.AbstractAnnotatedAlgorithm;
import org.n52.wps.server.LocalAlgorithmRepository;
import org.n52.wps.server.modules.LocalAlgorithmRepositoryCM;
import org.n52.wps.webapp.api.ConfigurationCategory;
import org.n52.wps.webapp.api.ConfigurationModule;
import org.n52.wps.webapp.service.ConfigurationService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This algorithm starts the CSWLoDEnabler.
 * 
 * @author BenjaminPross
 *
 */
@Algorithm(
        version = "1.1.0")
public class CSWLoDEnablerStarter extends AbstractAnnotatedAlgorithm {

    private static final Logger log = LoggerFactory.getLogger(CSWLoDEnablerStarter.class);

    private Properties props;

    private String projectUrl;

    private String projectName;

    private String projectShortname;

    private String uriBase;

    private String uriGraph;

    private String urlCSW;

    private GenericFileData report;

    public CSWLoDEnablerStarter() {

        props = new Properties();

        // constants
        props.setProperty("NS_GMD", "http://www.isotc211.org/2005/gmd");
        props.setProperty("NS_CSW", "http://www.opengis.net/cat/csw/2.0.2");
        props.setProperty("TEST_RECORD_ID", "glues:pik:metadata:dataset:noco2-echo-g-sresa1-annualcropyieldincreases");
        props.setProperty("SAVE_TO_FILE", "false");
        props.setProperty("ADD_TO_SERVER", "true");

        ConfigurationModule localAlgorithmConfigModule = WPSConfig.getInstance().getConfigurationModuleForClass(LocalAlgorithmRepository.class.getName(), ConfigurationCategory.REPOSITORY);

        ConfigurationService configurationService = WPSConfig.getInstance().getConfigurationManager().getConfigurationServices();

        String urlVirtuosoJdbc = (String) configurationService.getConfigurationEntry(localAlgorithmConfigModule, LocalAlgorithmRepositoryCM.virtuosoJDBCUrlKey).getValue();
        String virtuosoUser = (String) configurationService.getConfigurationEntry(localAlgorithmConfigModule, LocalAlgorithmRepositoryCM.virtuosoUserKey).getValue();
        String virtuosoPass = (String) configurationService.getConfigurationEntry(localAlgorithmConfigModule, LocalAlgorithmRepositoryCM.virtuosoPwdKey).getValue();

        // from WPS configuration
        props.setProperty("URL_VIRTUOSO_JDBC", urlVirtuosoJdbc);
        props.setProperty("VIRTUOSO_USER", virtuosoUser);
        props.setProperty("VIRTUOSO_PASS", virtuosoPass);

    }

    @ComplexDataOutput(
            identifier = "report", binding = GenericFileDataBinding.class)
    public GenericFileData getResult() {
        return report;
    }

    @LiteralDataInput(
            identifier = "projectUrl", binding = LiteralStringBinding.class, defaultValue = "http://nachhaltiges-landmanagement.de/en/scientific-coordination-glues/", minOccurs = 1, maxOccurs = 1)
    public void setProjectUrl(String projectUrl) {
        this.projectUrl = projectUrl;
    }

    @LiteralDataInput(
            identifier = "projectName", binding = LiteralStringBinding.class, defaultValue = "GLUES project", minOccurs = 1, maxOccurs = 1)
    public void setProjectName(String projectName) {
        this.projectName = projectName;
    }

    @LiteralDataInput(
            identifier = "projectShortname", binding = LiteralStringBinding.class, defaultValue = "GLUES", minOccurs = 1, maxOccurs = 1)
    public void setProjectShortname(String projectShortname) {
        this.projectShortname = projectShortname;
    }

    @LiteralDataInput(
            identifier = "uriBase", binding = LiteralStringBinding.class, defaultValue = "http://metadata.demo.52north.org/glues-lod", minOccurs = 1, maxOccurs = 1)
    public void setUriBase(String uriBase) {
        this.uriBase = uriBase;
    }

    @LiteralDataInput(
            identifier = "uriGraph", binding = LiteralStringBinding.class, minOccurs = 1, maxOccurs = 1)
    public void setUriGraph(String uriGraph) {
        this.uriGraph = uriGraph;
    }

    @LiteralDataInput(
            identifier = "urlCSW", binding = LiteralStringBinding.class, defaultValue = "http://catalog-glues.ufz.de/soapServices/CSWStartup", minOccurs = 1, maxOccurs = 1)
    public void setUrlCSW(String urlCSW) {
        this.urlCSW = urlCSW;
    }

    @Execute
    public void runAlgorithm() {

        // properties from process input
        props.setProperty("PROJECT_URL", projectUrl);
        props.setProperty("PROJECT_NAME", projectName);
        props.setProperty("PROJECT_SHORTNAME", projectShortname);
        props.setProperty("URI_BASE", uriBase);
        props.setProperty("URI_GRAPH", uriGraph);
        props.setProperty("URL_CSW", urlCSW);

        try {

            Configuration config = new Configuration(props);

            config.setProgressListener(new ProgressListener() {

                @Override
                public void updateProgress(Integer progress) {
                    update(progress);
                }
            });

            CSWLoDEnabler enabler = new CSWLoDEnabler(config);
            enabler.runOverAll();

            String reportString = enabler.getReport().extendedToString();
            
            File reportFile = File.createTempFile("cswlodreport", ".txt");
            
            BufferedWriter bufferedWriter = new BufferedWriter(new FileWriter(reportFile));
            
            bufferedWriter.write(reportString);
            
            bufferedWriter.close();
            
            report = new GenericFileData(reportFile, "text/plain");

        } catch (RuntimeException | IOException e) {
            log.error("Error running CSW to LOD", e);
        }
    }
}