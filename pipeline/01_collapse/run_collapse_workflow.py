import getpass
import os
from pathlib import Path
import time
import uuid
import pdb

from jobmon.client.tool import Tool
import pandas as pd

from global_helpers.helpers import load_config

## Setup
settings = load_config()
# NOTE!! - Requires jobmon-installer-ihme >= 10.7.1
user = getpass.getuser()
wf_uuid = uuid.uuid4()
tool = Tool(name='pcp_dla_collapse')
workflow = tool.create_workflow(
    name=f'pcp_dla_collapse_workflow_{wf_uuid}',
)
# User-editable local constants
PREP_VERSION = '2024-11-08'
RSCRIPT_EXEC = '/share/singularity-images/rstudio/shells/execRscript.sh'
STDOUT_PATH = ''
STDERR_PATH = ''
CLUSTER_PROJECT = 'proj_health_sys'

## NIDs for which to run workflow. Includes whether limited_use (1) or not (0)
# NOTE: UPDATE as needed with new NIDs list
dl_nids = pd.read_csv(os.path.join(settings['pcp_helpers'], 'nid_lists', 'maternal_delivery_location_nids_2024-07-31.csv'))

collapse_prep_path_j = os.path.join(settings['dla']['collapse_prep_j'], PREP_VERSION)
collapse_prep_path_l = os.path.join(settings['dla']['collapse_prep_l'], PREP_VERSION)
Path(collapse_prep_path_j).mkdir(parents=True, exist_ok=True)
Path(collapse_prep_path_l).mkdir(parents=True, exist_ok=True)
# J nids are limited_use==0
collapse_nids = dl_nids['nid'].unique().tolist()
# J extractions
extracted_j_path = os.path.join(settings['dla']['extract_j'], PREP_VERSION, 'maternal')
extracted_j_files = os.listdir(extracted_j_path)
extracted_j_nids = [int(x.split('_')[-2]) for x in extracted_j_files]
collapse_j_nids = [nid for nid in collapse_nids if nid in extracted_j_nids]
# LU extractions
extracted_l_path = os.path.join(settings['dla']['extract_l'], PREP_VERSION, 'maternal')
extracted_l_files = os.listdir(extracted_l_path)
extracted_l_nids = [int(x.split('_')[-2]) for x in extracted_l_files]
collapse_l_nids = [nid for nid in collapse_nids if nid in extracted_l_nids]
print(f'Ready to attempt collapse for {len(collapse_j_nids)} J-drive NIDs and {len(collapse_l_nids)} limited-use NIDs')
# collapse_j_nids = collapse_j_nids[:2] # DEV: Limit to fewer NIDs for testing
# collapse_l_nids = collapse_l_nids[:5] # DEV: Limit to fewer NIDs for testing
pdb.set_trace()

# Create task templates
collapse_prep_j_template = tool.get_task_template(
    default_compute_resources={
        'queue': 'all.q',
        'cores': 1,
        'memory': '8G',
        'runtime': '10m',
        'max_attempts': 1, # DEV: Default 3. Set to 1 for testing
        'stdout': os.path.join(STDOUT_PATH, 'pcp_dla_collapse_prep'),
        'stderr': os.path.join(STDERR_PATH, 'pcp_dla_collapse_prep'),
        'project': CLUSTER_PROJECT,
        'constraints': 'archive' # To request a J-drive access node
    },
    template_name='pcp_dla_collapse_prep_j_template',
    default_cluster_name='slurm',
    command_template=f'{RSCRIPT_EXEC} -s '
                     f'{Path(__file__).resolve().parent}/01_collapse_prep.R '
                     '--nid {nid} --multiple_extract_nid --version {version} -o',
    node_args=['nid'],
    task_args=['version'],
    op_args=[],
)
collapse_prep_l_template = tool.get_task_template(
    default_compute_resources={
        'queue': 'all.q',
        'cores': 1,
        'memory': '2G',
        'runtime': '10m',
        'max_attempts': 1, # DEV: Default 3. Set to 1 for testing
        'stdout': os.path.join(STDOUT_PATH, 'pcp_dla_collapse_prep'),
        'stderr': os.path.join(STDERR_PATH, 'pcp_dla_collapse_prep'),
        'project': CLUSTER_PROJECT,
        'constraints': 'archive' # To request a J-drive access node
    },
    template_name='pcp_dla_collapse_prep_l_template',
    default_cluster_name='slurm',
    command_template=f'{RSCRIPT_EXEC} -s '
                     f'{Path(__file__).resolve().parent}/01_collapse_prep.R '
                     '--nid {nid} --version {version} --limited_use -o',
    node_args=['nid'],
    task_args=['version'],
    op_args=[],
)
collapse_j_template = tool.get_task_template(
    default_compute_resources={
        'queue': 'all.q',
        'cores': 4,
        'memory': '16G',
        'runtime': '60m',
        'max_attempts': 1, # DEV: Default 3. Set to 1 for testing
        'stdout': os.path.join(STDOUT_PATH, 'pcp_dla_collapse'),
        'stderr': os.path.join(STDERR_PATH, 'pcp_dla_collapse'),
        'project': CLUSTER_PROJECT,
        'constraints': 'archive' # To request a J-drive access node
    },
    template_name='pcp_dla_collapse_j_template',
    default_cluster_name='slurm',
    command_template=f'{RSCRIPT_EXEC} -s '
                     f'{Path(__file__).resolve().parent}/02_collapse.R '
                     '--nid {nid} --version {version} -o',
    node_args=['nid'],
    task_args=['version'],
    op_args=[],
)
collapse_l_template = tool.get_task_template(
    default_compute_resources={
        'queue': 'all.q',
        'cores': 4,
        'memory': '16G',
        'runtime': '60m',
        'max_attempts': 1, # DEV: Default 3. Set to 1 for testing
        'stdout': os.path.join(STDOUT_PATH, 'pcp_dla_collapse'),
        'stderr': os.path.join(STDERR_PATH, 'pcp_dla_collapse'),
        'project': CLUSTER_PROJECT,
        'constraints': 'archive' # To request a J-drive access node
    },
    template_name='pcp_dla_collapse_l_template',
    default_cluster_name='slurm',
    command_template=f'{RSCRIPT_EXEC} -s '
                     f'{Path(__file__).resolve().parent}/02_collapse.R '
                     '--nid {nid} --version {version} --limited_use -o',
    node_args=['nid'],
    task_args=['version'],
    op_args=[],
)
collapse_concat_template = tool.get_task_template(
    default_compute_resources={
        'queue': 'all.q',
        'cores': 1,
        'memory': '2G',
        'runtime': '2m',
        'max_attempts': 1, # DEV: Default 3. Set to 1 for testing
        'stdout': os.path.join(STDOUT_PATH, 'pcp_dla_collapse'),
        'stderr': os.path.join(STDERR_PATH, 'pcp_dla_collapse'),
        'project': CLUSTER_PROJECT,
        'constraints': 'archive' # To request a J-drive access node
    },
    template_name='pcp_dla_collapse_concat_template',
    default_cluster_name='slurm',
    command_template=f'{RSCRIPT_EXEC} -s '
                     f'{Path(__file__).resolve().parent}/03_concat.R '
                     '--version {version} -o',
    node_args=[],
    task_args=['version'],
    op_args=[],
)

# Create tasks
collapse_prep_j_tasks = collapse_prep_j_template.create_tasks(
    name='collapse_prep_j_task',
    upstream_tasks=[],
    nid=collapse_j_nids,
    version=PREP_VERSION
)
workflow.add_tasks(collapse_prep_j_tasks)
collapse_prep_l_tasks = collapse_prep_l_template.create_tasks(
    name='collapse_prep_l_task',
    upstream_tasks=[],
    nid=collapse_l_nids,
    version=PREP_VERSION
)
workflow.add_tasks(collapse_prep_l_tasks)
collapse_j_tasks = collapse_j_template.create_tasks(
    name='collapse_j_task',
    upstream_tasks=collapse_prep_j_tasks,
    nid=collapse_j_nids,
    version=PREP_VERSION
)
workflow.add_tasks(collapse_j_tasks)
collapse_l_tasks = collapse_l_template.create_tasks(
    name='collapse_l_task',
    upstream_tasks=collapse_prep_l_tasks,
    nid=collapse_l_nids,
    version=PREP_VERSION
)
workflow.add_tasks(collapse_l_tasks)
collapse_concat_task = collapse_concat_template.create_task(
    name='collapse_concat_task',
    upstream_tasks=collapse_j_tasks + collapse_l_tasks,
    version=PREP_VERSION
)
workflow.add_tasks([collapse_concat_task])

# Connect dependencies
for nid in collapse_j_nids:
    node_args = {'nid': nid}
    single_collapse_prep_j_task = workflow.get_tasks_by_node_args('pcp_dla_collapse_prep_j_template', **node_args)
    single_collapse_j_task = workflow.get_tasks_by_node_args('pcp_dla_collapse_j_template', **node_args)
    single_collapse_j_task[0].add_upstream(single_collapse_prep_j_task[0])
    collapse_concat_task.add_upstream(single_collapse_j_task[0])
for nid in collapse_l_nids:
    node_args = {'nid': nid}
    single_collapse_prep_l_task = workflow.get_tasks_by_node_args('pcp_dla_collapse_prep_l_template', **node_args)
    single_collapse_l_task = workflow.get_tasks_by_node_args('pcp_dla_collapse_l_template', **node_args)
    single_collapse_l_task[0].add_upstream(single_collapse_prep_l_task[0])
    collapse_concat_task.add_upstream(single_collapse_l_task[0])

# Bind workflow
workflow.bind() # Calling workflow.bind() first just so that we can get the workflow id
print('Workflow creation complete.')
print(f'Running workflow with ID {workflow.workflow_id}.')
print('For full information see the Jobmon GUI:')
print(f'https://jobmon-gui.ihme.washington.edu/#/workflow/{workflow.workflow_id}/tasks')

# Run workflow
start_time = time.time()
status = workflow.run()
end_time = time.time()
mean_time = (end_time - start_time) / (len(collapse_j_nids) + len(collapse_l_nids))
print(f'Workflow {workflow.workflow_id} completed in {end_time - start_time:.0f} seconds with status {status}.')
print(f'Naively speaking this is {mean_time:.2f} seconds per NID.')
