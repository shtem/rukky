import os, re, subprocess

TEST_CORRECT_RESULTS = {
    "addNum": (210, float),
    "binarySearch": (6, float),
    "binaryTree": ("false", str),
    "countFrequency": ({1: 5, 2: 4, 3: 3, 4: 3, 5: 2}, float),
    "factorial": (120, float),
    "fibonacci": ([0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55], float),
    "lcm": (216, float),
    "palindrome": ("true", str),
    "primeInterval": (
        [907, 911, 919, 929, 937, 941, 947, 953, 967, 971, 977, 983, 991, 997],
        float,
    ),
    "quickSort": ([-1, 1, 2, 2, 4, 4, 5, 6, 6, 7, 8], float),
    "rotateArray": ([3, 4, 5, 6, 7, 1, 2], float),
    "unary": (4, float),
}  # each test and result corresponds to a file in the test folder


def get_test_result(filePath: str):
    scriptPath = os.path.join(os.getcwd(), "rukky", "rukky.py")
    command = f"python {scriptPath} -f {filePath}"
    result = subprocess.run(command, stdout=subprocess.PIPE)
    testOut = result.stdout.decode("utf-8").replace("\r\n", "").replace("\n", "")
    search = re.search(
        "result:\s+(.+)", testOut
    )  # get part of printed output where "result: <x>"
    _, match = search.group().split(":")  # match = <x>
    return match.strip()


def test_sucess():
    testDir = os.path.join(os.getcwd(), "rukky", "test", "files")
    testNamesandFiles = [
        (os.path.splitext(f)[0], os.path.join(testDir, f))
        for f in os.listdir(testDir)
        if os.path.isfile(os.path.join(testDir, f))
    ]
    for name, filePath in testNamesandFiles:
        testEntry = TEST_CORRECT_RESULTS.get(name, None)
        testResult = get_test_result(filePath=filePath)

        if testEntry == None:
            print(f"No correct result found for {name}")
            assert False

        correctResult, resultType = testEntry
        if isinstance(correctResult, list):
            testResult = testResult[1 : len(testResult) - 1].split(
                ","
            )  # remove "[" and "]" from string then split
            testResult = [resultType(x) for x in testResult]
        elif isinstance(correctResult, dict):
            testResult = testResult[1 : len(testResult) - 1].split(
                ","
            )  # remove "{" and "}" from string then split
            testResult = [x.split("->") for x in testResult]
            testResult = {resultType(x[0]): resultType(x[1]) for x in testResult}
        else:
            testResult = resultType(testResult)

        if correctResult == testResult:
            print(
                f"Test {name}: PASSED, test result: {testResult}, correct result: {correctResult}"
            )
        else:
            print(
                f"Test {name}: FAILED, test result: {testResult}, correct result: {correctResult}"
            )

        assert correctResult == testResult

    print("-------------\nAll tests: PASSED")


if __name__ == "__main__":
    test_sucess()
