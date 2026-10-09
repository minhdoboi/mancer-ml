import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import { Type } from "typebox";
import { mkdir, writeFile } from "node:fs/promises";
import { resolve, dirname } from "node:path";

export default function (pi: ExtensionAPI) {
  pi.registerTool({
    name: "bfl_image_gen",
    label: "BFL Image Generation",
    description:
      "Generate an image directly with the Black Forest Labs FLUX API and save it locally.",
    parameters: Type.Object({
      prompt: Type.String({
        description: "Detailed description of the image to generate",
      }),
      filename: Type.Optional(
        Type.String({
          description: "Output filename, for example image.png",
        }),
      ),
      aspect_ratio: Type.Optional(
       Type.Union([
          Type.Literal("1:1"),
          Type.Literal("16:9"),
          Type.Literal("9:16"),
          Type.Literal("4:3"),
          Type.Literal("3:4"),
          Type.Literal("3:2"),
          Type.Literal("2:3"),
        ], {
          description: "Format de l'image",
        }),
      ),
    }),

    async execute(_id, params, signal, _onUpdate, ctx) {
      const apiKey = process.env.BFL_API_KEY;

      if (!apiKey) {
        return {
          content: [{ type: "text", text: "BFL_API_KEY is not set." }],
          details: {},
          isError: true,
        };
      }

      const response = await fetch(
        "https://api.bfl.ai/v1/flux-3-image-klein",
        {
          method: "POST",
          headers: {
            "x-key": apiKey,
            "Content-Type": "application/json",
            accept: "application/json",
          },
          body: JSON.stringify({
            prompt: params.prompt,
            aspect_ratio: params.aspect_ratio
          }),
          signal,
        },
      );

      if (!response.ok) {
        throw new Error(
          `BFL request failed (${response.status}): ${await response.text()}`,
        );
      }

      const job = (await response.json()) as {
        id?: string;
        polling_url?: string;
      };

      if (!job.polling_url) {
        throw new Error("BFL did not return a polling_url.");
      }

      const pollingUrl = new URL(job.polling_url);

      //if (
      //  pollingUrl.protocol !== "https:" ||
      //  pollingUrl.hostname !== "api.bfl.ai"
      //) {
      //  throw new Error("Unexpected BFL polling URL.");
      // }

      let imageUrl: string | undefined;

      for (let attempt = 0; attempt < 120; attempt++) {
        signal.throwIfAborted();

        await new Promise((resolve) => setTimeout(resolve, 2000));

        const poll = await fetch(pollingUrl, {
          headers: {
            "x-key": apiKey,
            accept: "application/json",
          },
          signal,
        });

        if (!poll.ok) {
          throw new Error(`BFL polling failed (${poll.status}).`);
        }

        const result = (await poll.json()) as {
          status?: string;
          result?: { sample?: string };
          error?: string;
        };

        if (result.status === "Ready") {
          imageUrl = result.result?.sample;
          break;
        }

        if (result.status === "Error") {
          throw new Error(result.error || "BFL image generation failed.");
        }
      }

      if (!imageUrl) {
        throw new Error("BFL generation timed out after 4 minutes.");
      }

      const imageResponse = await fetch(imageUrl, { signal });

      if (!imageResponse.ok) {
        throw new Error("Could not download the generated image.");
      }

      const filename = params.filename || `flux-${Date.now()}.jpg`;
      const outputPath = resolve(ctx.cwd, "generated-images", filename);

      await mkdir(dirname(outputPath), { recursive: true });

      await writeFile(
        outputPath,
        Buffer.from(await imageResponse.arrayBuffer()),
      );

      return {
        content: [
          {
            type: "text",
            text: `Image generated successfully: ${outputPath}`,
          },
        ],
        details: { path: outputPath },
      };
    },
  });
}
